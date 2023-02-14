import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler, SourceTranspiler, SQLSourceTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";
import {SQLFromTranspiler} from "../expressions/sql_from";

function escapeRegExp(string: string) {
  return string.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"); // $& means the whole matched string
}

export class SelectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal, targetOverride?: string): Chunk {
    let target = "undefined";
    if (targetOverride) {
      target = targetOverride;
    } else if(node.findFirstExpression(abaplint.Expressions.Target)) {
      target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target)).getCode();
    }

    let select = "SELECT ";
    const fieldList = node.findFirstExpression(abaplint.Expressions.SQLFieldList)
      || node.findFirstExpression(abaplint.Expressions.SQLFieldListLoop);
    if (fieldList?.getChildren().length === 1) {
      select += fieldList.concatTokens();
    } else {
// add commas between field names, this is required for SQLite, and easy to remove in other clients?
      select += fieldList?.findAllExpressions(abaplint.Expressions.SQLField).map(e => e.concatTokens()).join(", ");
    }
    select += " ";

    const from = node.findFirstExpression(abaplint.Expressions.SQLFrom);
    if (from) {
      select += new SQLFromTranspiler().transpile(from, traversal).getCode();
    }

    let where: abaplint.Nodes.ExpressionNode | undefined = undefined;
    for(const sqlCond of node.findAllExpressions(abaplint.Expressions.SQLCond)){
      if(this.isWhereExpression(node, sqlCond)){
        where = sqlCond;
      }
    }
    if (where) {
      select += "WHERE " + traversal.traverse(where).getCode() + " ";
    }

    const upTo = node.findFirstExpression(abaplint.Expressions.SQLUpTo);
    if (upTo) {
      const s = upTo.findFirstExpression(abaplint.Expressions.SimpleSource3);
      if (s) {
        select += `UP TO " + ${new SourceTranspiler(true).transpile(s, traversal).getCode()} + " ROWS `;
      } else {
        select += upTo.concatTokens() + " ";
      }
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
      const fn = node.findFirstExpression(abaplint.Expressions.SQLForAllEntries)?.findDirectExpression(abaplint.Expressions.SQLSource);
      const faeTranspiled = new SQLSourceTranspiler().transpile(fn!, traversal).getCode();
      select = select.replace(new RegExp(" " + escapeRegExp(faeTranspiled!), "g"), " " + unique);
      select = select.replace(unique + ".get().table_line.get()", unique + ".get()");  // there can be only one?

      const code = `if (${faeTranspiled}.array().length === 0) {
  throw "FAE, todo, empty table";
} else {
  abap.statements.clear(${target});
  for await (const ${unique} of abap.statements.loop(${faeTranspiled})) {
    await abap.statements.select(${target}, {select: "${select.trim()}"${extra}}, {appending: true});
  }
  abap.statements.sort(${target});
  await abap.statements.deleteInternal(${target}, {adjacent: true});
  abap.builtin.sy.get().dbcnt.set(${target}.array().length);
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
        keys = tabl.listKeys(traversal.reg).map(k => k.toLowerCase());
      }
    }
    return keys;
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
    if(prevToken && prevToken.getStr().toUpperCase() === "WHERE"){
      return true;
    }else{
      return false;
    }
  }

}