import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SelectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target)).getCode();

    let select = "SELECT ";
    select += node.findFirstExpression(abaplint.Expressions.SQLFieldList)?.concatTokens() + " ";

    select += node.findFirstExpression(abaplint.Expressions.SQLFrom)?.concatTokens() + " ";

    if (node.concatTokens().toUpperCase().startsWith("SELECT SINGLE ")) {
      select += "LIMIT 1";
    }

    return new Chunk(`abap.statements.select(${target}, "${select.trim()}");`);
  }

}