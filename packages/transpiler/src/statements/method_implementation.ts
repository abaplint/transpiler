import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TranspileTypes} from "../types";
import {Traversal} from "../traversal";
import {ConstantTranspiler, FieldChainTranspiler} from "../expressions";
import {Chunk} from "../chunk";

export class MethodImplementationTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const token = node.findFirstExpression(abaplint.Expressions.MethodName)!.getFirstToken();
    let methodName = token.getStr();

    const scope = traversal.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error("MethodTranspiler, scope not found");
    } else if (scope.getIdentifier().sname !== methodName) {
      throw new Error("MethodTranspiler, wrong scope found, " + scope.getIdentifier().sname);
    }

    let after = "";

    const classDef = traversal.getClassDefinition(token);

    let unique = "";
    if (methodName.toUpperCase() === "CONSTRUCTOR" && classDef) {
// note that all ABAP identifiers are lower cased, sometimes the kernel does magic, so it needs to know the method input name
      unique = "INPUT";
      after = traversal.buildConstructorContents(scope.getParent(), classDef, unique);
      methodName = "constructor_";
    }

    const methodDef = this.findMethodParameters(scope);

    const vars = scope.getData().vars;
    for (const n in vars) {
      const identifier = vars[n];
      const varName = n.toLowerCase();
      if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodImporting)
          || identifier.getMeta().includes(abaplint.IdentifierMeta.MethodChanging)
          || identifier.getMeta().includes(abaplint.IdentifierMeta.MethodExporting)) {
        if (unique === "") {
          unique = "INPUT";
        }
        after = after + new TranspileTypes().declare(identifier) + "\n";
        if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodImporting) && identifier.getType().isGeneric() === false) {
          after += "if (" + unique + " && " + unique + "." + varName + ") {" + varName + ".set(" + unique + "." + varName + ");}\n";
        } else {
          after += "if (" + unique + " && " + unique + "." + varName + ") {" + varName + " = " + unique + "." + varName + ";}\n";
        }
        const parameterDefault = methodDef?.getParameterDefault(varName);
        if (parameterDefault) {
          let val = "";
          if (parameterDefault.get() instanceof abaplint.Expressions.Constant) {
            val = new ConstantTranspiler().transpile(parameterDefault, traversal).getCode();
          } else if (parameterDefault.get() instanceof abaplint.Expressions.FieldChain) {
            if (parameterDefault.getFirstToken().getStr().toLowerCase() === "abap_true") {
              val = "abap.builtin.abap_true";
            } else if (parameterDefault.getFirstToken().getStr().toLowerCase() === "abap_false") {
              val = "abap.builtin.abap_false";
            } else {
              // note: this can be difficult, the "def" might be from an interface, ie. a different scope than the method
//              try {
              val = new FieldChainTranspiler().transpile(parameterDefault, traversal, true, methodDef.getFilename()).getCode();
              if (val.startsWith(parameterDefault.getFirstToken().getStr()) === true) {
                val = "this." + val;
              }
                /*
              } catch (e) {
                val =
                console.dir(methodDef);
                console.dir(parameterDefault);
              }
              */
            }
          } else {
            throw "MethodImplementationTranspiler, unknown default param type";
          }
          after += "if (" + unique + " === undefined || " + unique + "." + varName + " === undefined) {" + varName + " = " + val + ";}\n";
        }
      } else if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        after = after + new TranspileTypes().declare(identifier) + "\n";
      }
    }

    if (after.length > 0) { // argh
      after = "\n" + after;
      after = after.substring(0, after.length - 1);
    }

    const method = this.findMethod(methodName, classDef, traversal);
    let staticMethod = "";

    methodName = methodName.replace("~", "$");

    if (method && method.isStatic()) {
      // in ABAP static methods can be called with instance arrows, "->"
      const className = scope.getParent()?.getIdentifier().sname?.toLowerCase();
      staticMethod = "async " + methodName + "(" + unique + ") {\n" +
        "return " + className + "." + methodName + "(" + unique + ");\n" +
        "}\n" + "static ";
    }

    const str = staticMethod + "async " + methodName + "(" + unique + ") {" + after;
    return new Chunk().append(str, node, traversal);
  }

  private findMethod(name: string, cdef: abaplint.IClassDefinition | undefined, traversal: Traversal) {
    if (cdef === undefined) {
      return undefined;
    }

    if (name.includes("~")) {
      const split = name.split("~");
      const intfName = split[0];
      name = split[1];
      const scope = traversal.findCurrentScopeByToken(cdef.getToken());
      const intf = scope?.findInterfaceDefinition(intfName);
      return intf?.getMethodDefinitions()?.getByName(name);
    } else {
      return cdef.getMethodDefinitions()?.getByName(name);
    }
  }

  private findMethodParameters(scope: abaplint.ISpaghettiScopeNode): abaplint.Types.MethodParameters | undefined {
    for (const r of scope.getData().references) {
      if (r.referenceType === abaplint.ReferenceType.MethodImplementationReference
          && r.resolved instanceof abaplint.Types.MethodDefinition) {
        return r.resolved.getParameters();
      }
    }
    return undefined;
  }

}