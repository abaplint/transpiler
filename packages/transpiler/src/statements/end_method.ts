import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class EndMethodTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const token = node.getFirstToken();

    const scope = traversal.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error("EndMethodTranspiler, scope not found");
    }

    let returning: string = "";

    let methodName: string | undefined = undefined;
    if (scope?.getIdentifier().stype === abaplint.ScopeType.Method) {
      methodName = scope?.getIdentifier().sname;
    }
    let className: string | undefined = undefined;
    if (scope?.getParent()?.getIdentifier().stype === abaplint.ScopeType.ClassImplementation) {
      className = scope?.getParent()?.getIdentifier().sname;
    }
    if (methodName && className) {
      const classDef = scope.findClassDefinition(className);
      const methodDef = classDef?.getMethodDefinitions().getByName(methodName);
      if (methodDef && methodDef.getExceptions().length > 0) {
        returning += "abap.builtin.sy.get().subrc.set(0);\n";
      }
    }

    const vars = scope.getData().vars;
    for (const n in vars) {
      const identifier = vars[n];
      if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        returning += "return " + n.toLowerCase() + ";\n";
      }
    }

    const data = scope.getIdentifier();
    if (data.stype === abaplint.ScopeType.Method && data.sname.toLowerCase() === "constructor") {
      returning += "return this;\n";
    }

    return new Chunk().append(returning + "}", node, traversal);
  }

}