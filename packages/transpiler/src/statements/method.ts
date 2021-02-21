import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TranspileTypes} from "../types";
import {Traversal} from "../traversal";

export class MethodTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const token = node.findFirstExpression(abaplint.Expressions.MethodName)!.getFirstToken();
    let methodName = token.getStr();

    const scope = traversal.findCurrentScope(token);
    if (scope === undefined) {
      throw new Error("MethodTranspiler, scope not found");
    } else if (scope.getIdentifier().sname !== methodName) {
      throw new Error("MethodTranspiler, wrong scope found, " + scope.getIdentifier().sname);
    }

    let after = "";

    const cdef = traversal.getClassDefinition(token);

    let unique = "";
    if (methodName.toUpperCase() === "CONSTRUCTOR" && cdef) {
// note that all ABAP identifiers are lower cased, sometimes the kernel does magic, so it needs to know the method input name
      unique = "INPUT";
      after = traversal.buildConstructorContents(scope.getParent(), cdef, unique);
      methodName = "constructor_";
    }

    const methoddef = this.findMethodParameters(scope);

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
        const def = methoddef?.getParameterDefault(varName);
        if (def) {
          after += "if (" + unique + " === undefined || " + unique + "." + varName + " === undefined) {" + varName + " = " + traversal.traverse(def) + ";}\n";
        }
      } else if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        after = after + new TranspileTypes().declare(identifier) + "\n";
      }
    }

    if (after.length > 0) { // argh
      after = "\n" + after;
      after = after.substring(0, after.length - 1);
    }

    // todo, does this work with interfaces?
    const defs = cdef?.getMethodDefinitions()?.getAll();
    let staticMethod = "";
    for (const m of defs || []) {
      if (m.getName().toUpperCase() === methodName.toUpperCase() && m.isStatic()) {
        // in ABAP static methods can be called with instance arrows, "->"
        const className = scope.getParent()?.getIdentifier().sname?.toLowerCase();
        staticMethod = "async " + methodName + "(" + unique + ") {\n" +
          "return " + className + "." + methodName + "(" + unique + ");\n" +
          "}\n" + "static ";
        break;
      }
    }

    return staticMethod + "async " + methodName.replace("~", "$") + "(" + unique + ") {" + after;
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