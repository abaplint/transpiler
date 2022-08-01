import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler, SimpleSource3Transpiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";

export class SelectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal, targetOverride?: string): Chunk {
    let target = "undefined";
    if (targetOverride) {
      target = targetOverride;
    } else if(node.findFirstExpression(abaplint.Expressions.Target)) {
      target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target)).getCode();
    }

    let select = "SELECT ";
    select += (node.findFirstExpression(abaplint.Expressions.SQLFieldList)?.concatTokens()
      || node.findFirstExpression(abaplint.Expressions.SQLFieldListLoop)?.concatTokens()) + " ";
    select += node.findFirstExpression(abaplint.Expressions.SQLFrom)?.concatTokens() + " ";

    let where;
    for(const sqlCond of node.findAllExpressions(abaplint.Expressions.SQLCond)){
      if(this.isWhereExpression(node,sqlCond)){
        where = sqlCond;
      }
    }
    if (where) {
      select += "WHERE " + this.concatCond(where, traversal) + " ";
    }
    const upTo = node.findFirstExpression(abaplint.Expressions.SQLUpTo);
    if (upTo) {
      select += upTo.concatTokens() + " ";
    }
    const orderBy = node.findFirstExpression(abaplint.Expressions.SQLOrderBy);
    if (orderBy) {
      select += orderBy.concatTokens() + " ";
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

    let runtimeOptions = "";
    if (node.concatTokens().toUpperCase().includes(" APPENDING TABLE ")) {
      runtimeOptions = `, {appending: true}`;
    }

    let extra = "";
    const keys = this.findKeys(node, traversal);
    if (keys.length > 0) {
      extra = `, primaryKey: ${JSON.stringify(keys)}`;
    }

    if (node.findFirstExpression(abaplint.Expressions.SQLForAllEntries)) {
      const unique = UniqueIdentifier.get();
      const faeName = node.findFirstExpression(abaplint.Expressions.SQLForAllEntries
      )?.findDirectExpression(abaplint.Expressions.SQLSource)?.concatTokens();
      select = select.replace(faeName!, unique);

      const code = `if (${faeName}.array().length === 0) {
  throw "FAE, todo, empty table";
} else {
  abap.statements.clear(${target});
  for (const ${unique} of abap.statements.loop(${faeName})) {
    await abap.statements.select(${target}, {select: "${select.trim()}"${extra}}, {appending: true});
  }
}`;
      return new Chunk().append(code, node, traversal);
    } else {
      return new Chunk().append(`await abap.statements.select(${target}, {select: "${
        select.trim()}"${extra}}${runtimeOptions});`, node, traversal);
    }
  }

  private findKeys(node: abaplint.Nodes.StatementNode, traversal: Traversal): string[] {
    let keys: string[] = [];
    const from = node.findAllExpressions(abaplint.Expressions.SQLFromSource).map(e => e.concatTokens());
    if (from.length === 1) {
      const tabl = traversal.findTable(from[0]);
      if (tabl) {
        keys = tabl.listKeys().map(k => k.toLowerCase());
      }
    }
    return keys;
  }

  private concatCond(cond: abaplint.Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";
    for (const c of cond.getChildren()) {
      if (c.get() instanceof abaplint.Expressions.SimpleSource3
          && c instanceof abaplint.Nodes.ExpressionNode
          && c.findDirectExpression(abaplint.Expressions.Constant) === undefined) {
        ret += " '\" + " + new SimpleSource3Transpiler(true).transpile(c, traversal).getCode() + " + \"'";
      } else if (c instanceof abaplint.Nodes.ExpressionNode) {
        ret += " " + this.concatCond(c, traversal);
      } else {
        ret += " " + c.concatTokens();
      }
    }
    return ret.trim();
  }

  private isWhereExpression(node: abaplint.Nodes.StatementNode, expression: abaplint.Nodes.ExpressionNode): boolean {
    // check if previous token before sqlCond is "WHERE". It could also be "ON" in case of join condition
    let prevToken;
    const sqlCondToken = expression.getFirstToken();
    for(const token of node.getTokens()){
      if(token.getStart() === sqlCondToken.getStart()){
        break;
      }
      prevToken = token;
    }
    if(prevToken && prevToken.getStr() === "WHERE"){
      return true;
    }else{
      return false;
    }
  }

}