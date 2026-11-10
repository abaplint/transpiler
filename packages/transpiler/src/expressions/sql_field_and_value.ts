import * as abaplint from "@abaplint/core";
import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SQLFieldNameTranspiler} from "./sql_field_name";

export class SQLFieldAndValueTranspiler implements IExpressionTranspiler {
  private readonly columns: Set<string> | undefined;
  private readonly statementStart: abaplint.Position | undefined;

  /** columns: the columns of the updated table, a plain name among them that is no data object is that column */
  public constructor(columns?: Set<string>, statementStart?: abaplint.Position) {
    this.columns = columns;
    this.statementStart = statementStart;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();

    chunk.appendString("\"");
    for (const n of node.getChildren()) {
      const concat = n.concatTokens();
      if (concat === "=") {
        chunk.appendString(" = ");
      } else if (n.get() instanceof abaplint.Expressions.SQLFieldName) {
        chunk.appendString(new SQLFieldNameTranspiler().transpile(n as Nodes.ExpressionNode, traversal).getCode());
      } else if (n.get() instanceof abaplint.Expressions.SQLSource && this.isColumn(n as Nodes.ExpressionNode, traversal)) {
        // SET f = g, where g is a column of the table: the column, as the database reads it
        chunk.appendString(new SQLFieldNameTranspiler().transpile(n as Nodes.ExpressionNode, traversal).getCode().toLowerCase());
      } else if (n.get() instanceof abaplint.Expressions.SQLSource) {
        const source = traversal.traverse(n);
        // todo: value conversion? abap vs db, traversal.isSQLConversion
        // todo: integers?
        chunk.appendString("'\" + abap.sqlEscape(" + source.getCode() + ".get()) + \"'");
      } else if (n instanceof abaplint.Nodes.TokenNode) {
        // operators/keywords, eg. the "+" in "field = field + value"
        chunk.appendString(" " + concat + " ");
      } else {
        chunk.appendString(traversal.traverse(n).getCode() + " ");
      }
    }
    chunk.appendString("\"");

    return chunk;
  }

  private isColumn(node: Nodes.ExpressionNode, traversal: Traversal): boolean {
    const plain = node.concatTokens();
    if (this.columns === undefined || /^[A-Za-z_][A-Za-z0-9_]*$/.test(plain) === false || !this.columns.has(plain.toLowerCase())) {
      return false;
    }
    const variable = traversal.findCurrentScopeByToken(node.getFirstToken())?.findVariable(plain);
    // abaplint declares an unknown name in SET at the UPDATE statement itself, that is no data object either
    return variable === undefined || (this.statementStart !== undefined && variable.getStart().equals(this.statementStart));
  }

}