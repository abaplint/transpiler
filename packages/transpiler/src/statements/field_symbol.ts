import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {FieldSymbolTranspiler as Expr} from "../expressions/field_symbol";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class FieldSymbolTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const name = node.findDirectExpression(abaplint.Expressions.FieldSymbol);
    if (name) {
      return new Chunk("let " + new Expr().transpile(name, traversal).getCode() + " = new abap.types.FieldSymbol();");
    }
    throw new Error("FieldSymbolTranspiler, name not found");
  }

}