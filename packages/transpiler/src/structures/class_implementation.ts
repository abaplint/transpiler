import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {TranspileTypes} from "../transpile_types";
import {Chunk} from "../chunk";

export class ClassImplementationTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {

    const ret = new Chunk();
    for (const c of node.getChildren()) {
      ret.appendChunk(traversal.traverse(c));
      if (c instanceof abaplint.Nodes.StatementNode
          && c.get() instanceof abaplint.Statements.ClassImplementation
          && this.hasConstructor(node) === false) {
        ret.appendString(this.buildConstructor(c, traversal));
      }
    }
    ret.appendString(this.buildStatic(node.findFirstExpression(abaplint.Expressions.ClassName), traversal));
    ret.appendString(this.buildTypes(node.findFirstExpression(abaplint.Expressions.ClassName), traversal));

    return ret;
  }

///////////////////////////////

  private hasConstructor(node: abaplint.Nodes.StructureNode): boolean {
    for (const m of node.findAllStatements(abaplint.Statements.MethodImplementation)) {
      const name = m.findFirstExpression(abaplint.Expressions.MethodName)?.getFirstToken().getStr();
      if (name?.toUpperCase() === "CONSTRUCTOR") {
        return true;
      }
    }
    return false;
  }

  /** Finds static attributes + constants including those from interfaces (from superclass is ingored) */
  private findStaticAttributes(cdef: abaplint.IClassDefinition, scope: abaplint.ISpaghettiScopeNode, traversal: Traversal){

    const ret: {identifier: abaplint.Types.ClassAttribute | abaplint.Types.ClassConstant, prefix: string}[] = [];

    ret.push(...cdef.getAttributes().getStatic().map(a => {return {identifier: a, prefix: ""};}));
    ret.push(...cdef.getAttributes().getConstants().map(a => {return {identifier: a, prefix: ""};}));

    const implementing = [...cdef.getImplementing()];
    while (implementing.length > 0) {
      const i = implementing.shift();
      if (i === undefined) {
        break;
      }

      const intf = traversal.findInterfaceDefinition(i.name, scope);
      if (intf === undefined) {
        continue;
      }

      implementing.push(...intf.getImplementing());

      ret.push(...intf.getAttributes().getStatic().map(a => {
        return {identifier: a, prefix: intf.getName().toLowerCase() + "$"};}));
      ret.push(...intf.getAttributes().getConstants().map(a => {
        return {identifier: a, prefix: intf.getName().toLowerCase() + "$"};}));
    }

    return ret;
  }

  private buildTypes(node: abaplint.Nodes.ExpressionNode | undefined, traversal: Traversal): string {
    if (node === undefined) {
      return "";
    }
    const cdef = traversal.getClassDefinition(node.getFirstToken());
    if (cdef === undefined) {
      return "ERROR_CDEF_NOT_FOUND";
    }

    const prefix = Traversal.escapeNamespace(cdef.getName().toLowerCase()) + ".";
    let ret = "";
    for (const ty of cdef.getTypeDefinitions().getAll()) {
      ret += new TranspileTypes().declareStaticSkipVoid(prefix, ty.type);
    }
    return ret;
  }

  /** this builds the part after the class, containing the static variables/constants */
  private buildStatic(node: abaplint.Nodes.ExpressionNode | undefined, traversal: Traversal): string {
    if (node === undefined) {
      return "";
    }
    const cdef = traversal.getClassDefinition(node.getFirstToken());
    if (cdef === undefined) {
      return "ERROR_CDEF_NOT_FOUND";
    }
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    if (scope === undefined) {
      return "ERROR_SCOPE_NOT_FOUND";
    }

    let ret = "";
    const clasName = node.getFirstToken().getStr().toLowerCase();
    const staticAttributes = this.findStaticAttributes(cdef, scope, traversal);
    for (const attr of staticAttributes) {
      const name = Traversal.escapeNamespace(clasName) + "." + Traversal.escapeNamespace(attr.prefix) + Traversal.escapeNamespace(attr.identifier.getName().toLowerCase());
      ret += name + " = " + new TranspileTypes().toType(attr.identifier.getType()) + ";\n";

      ret += traversal.setValues(attr.identifier, name);
    }

    for (const alias of cdef.getAliases().getAll()) {
      const isStatic = staticAttributes.some(s => s.prefix.replace("$", "~") + s.identifier.getName() === alias.getComponent());
      if (isStatic === false) {
        continue;
      }
      ret += Traversal.escapeNamespace(clasName) + "." + alias.getName().toLowerCase() + " = " + Traversal.escapeNamespace(clasName) + "." + Traversal.escapeNamespace(alias.getComponent().replace("~", "$")) + ";\n";
    }

    // this is not correct, ABAP does not invocate the class constructor at require time,
    // but this will probably work
    if (traversal.getCurrentObject().getType() === "CLAS") {
// gather call of all class constructors together for a global class, also local classes
// as they might use the global class, except for local testclass constructors
      if (cdef.isGlobal() === true) {
        for (const f of traversal.getCurrentObject().getABAPFiles()) {
          for (const def of f.getInfo().listClassDefinitions()) {
            if (def.isForTesting === true) {
              continue;
            } else if (def.methods.some(m => m.name.toLowerCase() === "class_constructor")) {
              ret += "await " + traversal.lookupClassOrInterface(def.name, node.getFirstToken()) + ".class_constructor();\n";
            }
          }
        }
      }
      if (cdef.isGlobal() === false && cdef.isForTesting() === true && cdef.getMethodDefinitions().getByName("class_constructor")) {
        ret += "await " + Traversal.escapeNamespace(node.getFirstToken().getStr().toLowerCase()) + ".class_constructor();\n";
      }
    } else if (cdef.getMethodDefinitions().getByName("class_constructor")) {
      ret += "await " + Traversal.escapeNamespace(node.getFirstToken().getStr().toLowerCase()) + ".class_constructor();\n";
    }

    return ret;
  }

  private buildConstructor(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());

    const token = node.findFirstExpression(abaplint.Expressions.ClassName)?.getFirstToken();
    if (token === undefined) {
      throw "buildConstructorTokenNotFound";
    }
    const cdef = traversal.getClassDefinition(token);
    if (cdef === undefined) {
      throw "buildConstructorCDEFNotFound";
    }

    const ret = traversal.buildConstructorContents(scope, cdef);
    if (ret === "") {
      return ret;
    }

    return "async constructor_(INPUT) {\n" + ret + "return this;\n}\n";
  }

}