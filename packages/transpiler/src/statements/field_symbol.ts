import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {FieldSymbolTranspiler as Expr} from "../expressions/field_symbol";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";

export class FieldSymbolTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const name = node.findDirectExpression(abaplint.Expressions.FieldSymbol);

    const token = name?.getFirstToken();
    if (token === undefined) {
      throw new Error("FieldSymbolTranspiler, token not found");
    }

    const scope = traversal.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error("FieldSymbolTranspiler, scope not found");
    }

    const found = scope.findVariable(token.getStr());
    if (found === undefined) {
      throw new Error("FieldSymbolTranspiler, var not found, \"" + token.getStr() + "\"");
    }

    if (name) {
      return new Chunk()
        .append("let ", node, traversal)
        .appendChunk(new Expr().transpile(name, traversal))
        .append(" = new abap.types.FieldSymbol(" + new TranspileTypes().toType(found.getType()) + ");", node.getLastToken(), traversal);
    }
    throw new Error("FieldSymbolTranspiler, name not found");
  }

}