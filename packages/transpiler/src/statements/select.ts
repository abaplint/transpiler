import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler} from "../expressions";

export class SelectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target)).getCode();

    let select = "SELECT ";
    select += node.findFirstExpression(abaplint.Expressions.SQLFieldList)?.concatTokens() + " ";
    select += node.findFirstExpression(abaplint.Expressions.SQLFrom)?.concatTokens() + " ";

    const where = node.findFirstExpression(abaplint.Expressions.SQLCond);
    if (where) {
      select += "WHERE " + where.concatTokens() + " ";
    }
    const orderBy = node.findFirstExpression(abaplint.Expressions.SQLOrderBy);
    if (orderBy) {
      select += orderBy.concatTokens() + " ";
    }
    const upTo = node.findFirstExpression(abaplint.Expressions.SQLUpTo);
    if (upTo) {
      select += upTo.concatTokens() + " ";
    }

    for (const d of node.findAllExpressionsRecursive(abaplint.Expressions.Dynamic)) {
      const chain = d.findFirstExpression(abaplint.Expressions.FieldChain);
      if (chain) {
        const code = new FieldChainTranspiler(true).transpile(chain, traversal).getCode();
        const search = d.concatTokens();
        select = select.replace(search, `" + ${code} + "`);
      }
    }

    if (node.concatTokens().toUpperCase().startsWith("SELECT SINGLE ")) {
      select += "UP TO 1 ROWS";
    }

    return new Chunk().append(`await abap.statements.select(${target}, {select: "${select.trim()}"});`, node, traversal);
  }

}