import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {ClassImplementationTranspiler as ClassImplementationStatementTranspiler} from "../statements/class_implementation";

const DEFAULT_CONSTRUCTOR = "async constructor_(INPUT) {\nif (super.constructor_) { await super.constructor_(INPUT); }\nreturn this;\n}\n";

export class ClassImplementationTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const token = node.findFirstExpression(abaplint.Expressions.ClassName)?.getFirstToken();
    if (token === undefined) {
      throw new Error("ClassImplementationTranspiler, class name not found");
    }
    const methods = node.findAllStatements(abaplint.Statements.MethodImplementation);

    const ret = new Chunk();

    for (const c of node.getChildren()) {
      if (c.get() instanceof abaplint.Statements.EndClass) {
        ret.appendString(this.buildPartiallyImplementedStubs(token, methods, traversal));
      }
      ret.appendChunk(traversal.traverse(c));
      if (c.get() instanceof abaplint.Statements.ClassImplementation) {
        ret.appendString(this.buildConstructor(token, traversal));
        if (this.hasConstructor(methods) === false) {
          ret.appendString(DEFAULT_CONSTRUCTOR);
        }
      }
    }

    ret.appendString(this.buildStatic(token, traversal));
    ret.appendString(this.buildTypes(token, traversal));

    return ret;
  }

  /** a class with a DEFINITION and no IMPLEMENTATION, valid ABAP when the class has no
   * methods to implement, eg. an exception class or an abstract class with only abstract
   * methods: the same output as for an empty CLASS ... IMPLEMENTATION. ENDCLASS. */
  public transpileWithoutImplementation(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const token = node.findFirstExpression(abaplint.Expressions.ClassName)?.getFirstToken();
    if (token === undefined) {
      throw new Error("ClassImplementationTranspiler, class name not found");
    }

    const ret = new Chunk();
    ret.append(new ClassImplementationStatementTranspiler().buildHeader(token, traversal) + "\n", node, traversal);
    ret.appendString(this.buildConstructor(token, traversal));
    ret.appendString(DEFAULT_CONSTRUCTOR);
    ret.appendString(this.buildPartiallyImplementedStubs(token, [], traversal));
    ret.append("}\n" + traversal.registerClassOrInterface(traversal.getClassDefinition(token)) + "\n",
               node.getLastToken(), traversal);
    ret.appendString(this.buildStatic(token, traversal));
    ret.appendString(this.buildTypes(token, traversal));
    return ret;
  }

///////////////////////////////

  /** INTERFACES ... PARTIALLY IMPLEMENTED: the interface methods the test class
   * does not implement exist on a system and raise CX_SY_DYN_CALL_ILLEGAL_METHOD
   * when called. Emits a method of that shape under the name the implementation
   * would have had, so the call ends in that exception and not in a TypeError.
   * Also the optional interface methods, METHODS ... DEFAULT IGNORE | DEFAULT FAIL, a class
   * does not implement: IGNORE behaves like an empty method, FAIL raises CX_SY_DYN_CALL_ILLEGAL_METHOD */
  private buildPartiallyImplementedStubs(token: abaplint.Token, methods: readonly abaplint.Nodes.StatementNode[],
                                         traversal: Traversal): string {
    const cdef = traversal.getClassDefinition(token);
    const scope = traversal.findCurrentScopeByToken(token);
    if (cdef === undefined || scope === undefined) {
      return "";
    }
    const partial = cdef.getImplementing().filter(i => i.partial === true);
    const others = cdef.getImplementing().filter(i => i.partial !== true);
    if (partial.length === 0 && others.length === 0) {
      return "";
    }

    // the methods the class implements itself, by the emitted name
    const implemented = new Set<string>();
    for (const m of methods) {
      const name = m.findFirstExpression(abaplint.Expressions.MethodName)?.getFirstToken().getStr();
      if (name !== undefined) {
        implemented.add(Traversal.escapeNamespace(name.replace("~", "$").toLowerCase())!);
      }
    }

    // an interface a superclass implements is inherited, with the real implementations,
    // a stub for one of its methods would override those
    const inherited = new Set<string>();
    let sup = cdef.getSuperClass();
    while (sup !== undefined) {
      const sdef = traversal.findClassDefinition(sup, scope);
      for (const name of this.findIncludedInterfaces(sdef?.getImplementing().map(i => i.name) || [], scope, traversal)) {
        inherited.add(name);
      }
      sup = sdef?.getSuperClass();
    }

    let ret = "";
    const partialNames = this.findIncludedInterfaces(partial.map(i => i.name), scope, traversal);
    const otherNames = this.findIncludedInterfaces(others.map(i => i.name), scope, traversal)
      .filter(n => partialNames.includes(n) === false);
    for (const name of [...partialNames, ...otherNames]) {
      if (inherited.has(name)) {
        continue;
      }
      const intf = traversal.findInterfaceDefinition(name, scope);
      if (intf === undefined) {
        continue;
      }
      const prefix = Traversal.escapeNamespace(intf.getName().toLowerCase()) + "$";
      for (const m of intf.getMethodDefinitions().getAll()) {
        const emitted = prefix + Traversal.escapeNamespace(m.getName().toLowerCase());
        if (implemented.has(emitted)) {
          continue;
        }
        const optional = this.findOptionalDefault(m, traversal);
        if (optional === "IGNORE") {
          implemented.add(emitted);
          ret += this.buildIgnoredMethod(emitted, m, token.getStr().toLowerCase(), traversal);
        } else if (optional === "FAIL" || partialNames.includes(name)) {
          implemented.add(emitted);
          ret += this.buildNotImplementedMethod(emitted, m, token.getStr().toLowerCase(), intf.getName(), token, traversal);
        }
      }
    }
    return ret;
  }

  /** METHODS ... DEFAULT IGNORE | DEFAULT FAIL of an interface method, read from its declaration */
  private findOptionalDefault(m: abaplint.IMethodDefinition, traversal: Traversal): "IGNORE" | "FAIL" | undefined {
    const file = traversal.reg.getFileByName(m.getFilename());
    const obj = file ? traversal.reg.findObjectForFile(file) : undefined;
    if (!(obj instanceof abaplint.ABAPObject)) {
      return undefined;
    }
    const start = m.getStart();
    const statement = obj.getABAPFileByName(m.getFilename())?.getStatements().find(s =>
      s.get() instanceof abaplint.Statements.MethodDef
      && s.getStart().isBefore(start) && s.getEnd().isAfter(start));
    const match = statement?.concatTokens().toUpperCase().match(/ DEFAULT (IGNORE|FAIL)\b/);
    return match ? match[1] as "IGNORE" | "FAIL" : undefined;
  }

  /** an optional interface method, DEFAULT IGNORE, the class does not implement: it does
   * nothing, outputs passed by value and the returning parameter come back initial */
  private buildIgnoredMethod(emitted: string, m: abaplint.IMethodDefinition, className: string, traversal: Traversal): string {
    const parameters = m.getParameters();
    const unique = parameters.getAll().length > 0 ? "INPUT" : "";
    let body = "";
    for (const p of parameters.getExporting()) {
      if (p.getMeta().includes(abaplint.IdentifierMeta.PassByValue)) {
        body += `INPUT?.${p.getName().toLowerCase()}?.clear();\n`;
      }
    }
    const returning = parameters.getReturning();
    if (returning !== undefined) {
      body += `return ${traversal.toType(returning.getType())};\n`;
    }

    let ret = "";
    if (m.isStatic() === true) {
      ret += "async " + emitted + "(" + unique + ") {\n" +
        "return " + Traversal.escapeNamespace(className) + "." + emitted + "(" + unique + ");\n" +
        "}\n" + "static ";
    }
    ret += "async " + emitted + "(" + unique + ") {\n" + body + "}\n";
    return ret;
  }

  /** the interfaces, upper cased, with the interfaces they include, recursively */
  private findIncludedInterfaces(names: string[], scope: abaplint.ISpaghettiScopeNode, traversal: Traversal): string[] {
    const ret: string[] = [];
    const queue = [...names];
    while (queue.length > 0) {
      const name = queue.shift()!.toUpperCase();
      if (ret.includes(name)) {
        continue;
      }
      ret.push(name);
      const intf = traversal.findInterfaceDefinition(name, scope);
      queue.push(...(intf?.getImplementing().map(i => i.name) || []));
    }
    return ret;
  }

  /** the method a system generates for an interface method the test class leaves
   * out under PARTIALLY IMPLEMENTED: calling it raises CX_SY_DYN_CALL_ILLEGAL_METHOD
   * (ABAP keyword documentation, INTERFACES - PARTIALLY IMPLEMENTED), the class and
   * method named, instead of a JavaScript TypeError no CATCH sees */
  private buildNotImplementedMethod(emitted: string, m: abaplint.IMethodDefinition, className: string,
                                    interfaceName: string, token: abaplint.Token, traversal: Traversal): string {
    const unique = m.getParameters().getAll().length > 0 ? "INPUT" : "";
    const exception = traversal.lookupClassOrInterface("'CX_SY_DYN_CALL_ILLEGAL_METHOD'", token, true);
    const classUpper = className.toUpperCase();
    const methodUpper = interfaceName.toUpperCase() + "~" + m.getName().toUpperCase();
    const notFound = `CX_SY_DYN_CALL_ILLEGAL_METHOD not found, ${classUpper}->${methodUpper} is not implemented`;
    const body = `if (${exception} === undefined) { throw new Error("${notFound}"); }\n` +
      `throw await new ${exception}().constructor_({` +
      `classname: new abap.types.String().set("${classUpper}"), ` +
      `methodname: new abap.types.String().set("${methodUpper}")});\n`;

    let ret = "";
    if (m.isStatic() === true) {
      // in ABAP static methods can be called with instance arrows, "->"
      ret += "async " + emitted + "(" + unique + ") {\n" +
        "return " + Traversal.escapeNamespace(className) + "." + emitted + "(" + unique + ");\n" +
        "}\n" + "static ";
    }
    ret += "async " + emitted + "(" + unique + ") {\n" + body + "}\n";
    return ret;
  }

  private hasConstructor(methods: readonly abaplint.Nodes.StatementNode[]): boolean {
    for (const m of methods) {
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

  private buildTypes(token: abaplint.Token, traversal: Traversal): string {
    const cdef = traversal.getClassDefinition(token);
    if (cdef === undefined) {
      return "ERROR_CDEF_NOT_FOUND";
    }

    const prefix = Traversal.escapeNamespace(cdef.getName().toLowerCase()) + ".";
    let ret = "";
    for (const ty of cdef.getTypeDefinitions().getAll()) {
      ret += traversal.declareStaticSkipVoid(prefix, ty.type);
    }
    return ret;
  }

  private buildPrivate(token: abaplint.Token, traversal: Traversal): string {
    const cdef = traversal.getClassDefinition(token);
    if (cdef === undefined) {
      return "ERROR_CDEF_NOT_FOUND";
    }

    let ret = "";
    for (const attr of cdef.getAttributes().getAll()) {
      if (attr.getVisibility() === abaplint.Visibility.Private) {
        ret += "#" + Traversal.escapeNamespace(attr.getName().toLowerCase()) + ";\n";
      }
    }
    return ret;
  }

  /** this builds the part after the class, containing the static variables/constants */
  private buildStatic(token: abaplint.Token, traversal: Traversal): string {
    const cdef = traversal.getClassDefinition(token);
    if (cdef === undefined) {
      return "ERROR_CDEF_NOT_FOUND";
    }
    const scope = traversal.findCurrentScopeByToken(token);
    if (scope === undefined) {
      return "ERROR_SCOPE_NOT_FOUND";
    }

    let ret = "";
    const clasName = token.getStr().toLowerCase();
    const staticAttributes = this.findStaticAttributes(cdef, scope, traversal);
    for (const attr of staticAttributes) {
      const name = Traversal.escapeNamespace(clasName) + "." + Traversal.escapeNamespace(attr.prefix) + Traversal.escapeNamespace(attr.identifier.getName().toLowerCase());
      ret += name + " = " + traversal.toType(attr.identifier.getType()) + ";\n";

      ret += traversal.setValues(attr.identifier, name);
    }

    for (const alias of cdef.getAliases()) {
      // note: the component is the raw source casing, so compare case insensitive
      const component = alias.getComponent().toLowerCase();
      const isStatic = staticAttributes.some(s =>
        (s.prefix.replace("$", "~") + s.identifier.getName()).toLowerCase() === component);
      if (isStatic === false) {
        continue;
      }
      ret += Traversal.escapeNamespace(clasName) + "." + alias.getName().toLowerCase() + " = " + Traversal.escapeNamespace(clasName) + "." + Traversal.escapeNamespace(component.replace("~", "$")) + ";\n";
    }

    for (const e of cdef.getEvents()) {
      if (e.isStatic() === false) {
        continue;
      }
      const fname = Traversal.escapeNamespace(e.getName().toLowerCase());
      const name = traversal.buildInternalName(clasName, cdef);
      ret += Traversal.escapeNamespace(clasName) + "." + fname + " = {\"EVENT_NAME\": \"" + e.getName().toUpperCase() + "\", \"EVENT_CLASS\": \"" + name + "\"};\n";
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
              ret += "await " + traversal.lookupClassOrInterface(def.name, token) + ".class_constructor();\n";
            }
          }
        }
      }
      if (cdef.isGlobal() === false && cdef.isForTesting() === true && cdef.getMethodDefinitions().getByName("class_constructor")) {
        ret += "await " + Traversal.escapeNamespace(clasName) + ".class_constructor();\n";
      }
    } else if (cdef.getMethodDefinitions().getByName("class_constructor")) {
      ret += "await " + Traversal.escapeNamespace(clasName) + ".class_constructor();\n";
    }

    return ret;
  }

  private buildConstructor(token: abaplint.Token, traversal: Traversal): string {
    const scope = traversal.findCurrentScopeByToken(token);

    const cdef = traversal.getClassDefinition(token);
    if (cdef === undefined) {
      throw "buildConstructorCDEFNotFound, " + token.getStr();
    }

    const ret = traversal.buildConstructorContents(scope, cdef);
    if (ret === "") {
      return ret;
    }

    const privates = this.buildPrivate(token, traversal);

// note: for CALL TRANSFORMATION, its nice that the values are initialized by the JS constructor,
    return privates + "constructor() {\n" + ret + "}\n";
  }

}