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
    const vars = scope.getData().vars;
    for (const n in vars) {
      const identifier = vars[n];
      if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        returning = "return " + n.toLowerCase() + ";\n";
      }
    }

    const data = scope.getIdentifier();
    if (data.stype === abaplint.ScopeType.Method && data.sname.toLowerCase() === "constructor") {
      returning = "return this;\n";
    }

    return new Chunk(returning + "}");
  }

}