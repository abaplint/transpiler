import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TranspileTypes} from "../transpile_types";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class TablesTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const token = node.findFirstExpression(abaplint.Expressions.Field)?.getFirstToken();
    if (token === undefined) {
      throw new Error("TablesTranspiler, token not found");
    }

    const scope = traversal.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error("TablesTranspiler, scope not found");
    }

    const found = scope.findVariable(token.getStr());
    if (found === undefined) {
      throw new Error("TablesTranspiler, var not found, \"" + token.getStr() + "\"");
    }

    const ret = new Chunk()
      .appendString("let ")
      .append(found.getName().toLowerCase(), token, traversal)
      .appendString(" = " + TranspileTypes.toType(found.getType()))
      .append(";", node.getLastToken(), traversal);

    return ret;
  }

}