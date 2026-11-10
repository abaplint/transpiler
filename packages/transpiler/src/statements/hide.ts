import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class HideTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    // HIDE of a local field of a procedure is a runtime error
    const token = node.findDirectExpression(abaplint.Expressions.Source)?.getFirstToken();
    const scope = token ? traversal.findCurrentScopeByToken(token) : undefined;
    const stype = scope?.getIdentifier().stype;
    if (token && scope && (stype === abaplint.ScopeType.Form || stype === abaplint.ScopeType.Method
        || stype === abaplint.ScopeType.FunctionModule)) {
      const vars = scope.getData().vars;
      const name = token.getStr();
      if (vars[name.toLowerCase()] !== undefined || vars[name.toUpperCase()] !== undefined) {
        return new Chunk().append(`throw new Error("HIDE_NO_LOCAL, HIDE of the local field ${name.toUpperCase()}");`, node, traversal);
      }
    }
    // otherwise HIDE keeps a field's value for the list line, for the line selection of an interactive list
    // (AT LINE-SELECTION), it does not change the list itself
    return new Chunk("");
  }

}
