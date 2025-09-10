import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";

export class RangesTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const token = node.findFirstExpression(abaplint.Expressions.DefinitionName)?.getFirstToken();
    if (token === undefined) {
      throw new Error("RangesTranspiler, token not found");
    }

    const scope = traversal.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error("RangesTranspiler, scope not found: " + node.concatTokens());
    }

    const found = scope.findVariable(token.getStr());
    if (found === undefined) {
      throw new Error("RangesTranspiler, var not found, \"" + token.getStr() + "\", " + traversal.getFilename() + ", line: " + token.getRow());
    }

    const ret = new Chunk()
      .appendString("let ")
      .appendString(Traversal.prefixVariable(Traversal.escapeNamespace(found.getName().toLowerCase())))
      .appendString(" = " + TranspileTypes.toType(found.getType()))
      .appendString(";");

    return ret;
  }

}