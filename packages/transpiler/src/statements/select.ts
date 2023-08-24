import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler, SourceTranspiler, SQLCondTranspiler, SQLFieldTranspiler, SQLSourceTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";
import {SQLFromTranspiler} from "../expressions/sql_from";

function escapeRegExp(string: string) {
  return string.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"); // $& means the whole matched string
}

// TODO: currently SELECT into are always handled as CORRESPONDING
export class SelectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal, targetOverride?: string): Chunk {
    let target = "undefined";
    if (targetOverride) {
      // SelectLoop structure uses override
      target = targetOverride;
    } else if (node.findFirstExpression(abaplint.Expressions.SQLIntoTable)) {
      target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target)).getCode();
    } else if (node.findFirstExpression(abaplint.Expressions.SQLIntoStructure)) {
      target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.SQLIntoStructure)).getCode();
    }

    let select = "SELECT ";
    const fieldList = node.findFirstExpression(abaplint.Expressions.SQLFieldList)
      || node.findFirstExpression(abaplint.Expressions.SQLFieldListLoop);
    const fields: string[] = [];
    for (const f of fieldList?.getChildren() || []) {
      if (f instanceof abaplint.Nodes.ExpressionNode && f.get() instanceof abaplint.Expressions.SQLField) {
        const code = new SQLFieldTranspiler().transpile(f, traversal).getCode();
        fields.push(code);
      } else {
        fields.push(f.concatTokens());
      }
    }
    select += fields.join(", ") + " ";

    const from = node.findFirstExpression(abaplint.Expressions.SQLFrom);
    if (from) {
      select += new SQLFromTranspiler().transpile(from, traversal).getCode();
    }

    const {table, keys} = this.findTable(node, traversal);

    let where: abaplint.Nodes.ExpressionNode | undefined = undefined;
    for(const sqlCond of node.findAllExpressions(abaplint.Expressions.SQLCond)){
      if(this.isWhereExpression(node, sqlCond)){
        where = sqlCond;
      }
    }
    if (where) {
      select += "WHERE " + new SQLCondTranspiler().transpile(where, traversal, table).getCode() + " ";
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

    const concat = node.concatTokens().toUpperCase();
    if (concat.startsWith("SELECT SINGLE ")) {
      select += "UP TO 1 ROWS";
    }

    let runtimeOptions = "";
    const runtimeOptionsList: string[] = [];
    if (concat.includes(" APPENDING TABLE ") || concat.includes(" APPENDING CORRESPONDING FIELDS OF TABLE ")) {
      runtimeOptionsList.push(`appending: true`);
    }
    if (runtimeOptionsList.length > 0) {
      runtimeOptions = `, {` + runtimeOptionsList.join(", ") + `}`;
    }

    let extra = "";
    if (keys.length > 0) {
      extra = `, primaryKey: ${JSON.stringify(keys)}`;
    }

    if (node.findFirstExpression(abaplint.Expressions.SQLForAllEntries)) {
      const unique = UniqueIdentifier.get();
      const fn = node.findFirstExpression(abaplint.Expressions.SQLForAllEntries)?.findDirectExpression(abaplint.Expressions.SQLSource);
      const faeTranspiled = new SQLSourceTranspiler().transpile(fn!, traversal).getCode();
      select = select.replace(new RegExp(" " + escapeRegExp(faeTranspiled!), "g"), " " + unique);
      select = select.replace(unique + ".get().table_line.get()", unique + ".get()");  // there can be only one?

      let by = `Object.keys(${target}.getRowType().get())`;
      if (keys.length > 0) {
        by = JSON.stringify(keys);
      }

      const code = `if (${faeTranspiled}.array().length === 0) {
  throw "FAE, todo, empty table";
} else {
  abap.statements.clear(${target});
  for await (const ${unique} of abap.statements.loop(${faeTranspiled})) {
    await abap.statements.select(${target}, {select: "${select.trim()}"${extra}}, {appending: true});
  }
  if (!(${target} instanceof abap.types.HashedTable) && ${target}.getOptions()?.primaryKey?.type !== "SORTED") {
    abap.statements.sort(${target}, {by: ${by}.map(k => { return {component: k}; })});
    await abap.statements.deleteInternal(${target}, {adjacent: true, by: ${by}});
  }
  abap.builtin.sy.get().dbcnt.set(${target}.getArrayLength());
}`;
      return new Chunk().append(code, node, traversal);
    } else {
      return new Chunk().append(`await abap.statements.select(${target}, {select: "${
        select.trim()}"${extra}}${runtimeOptions});`, node, traversal);
    }
  }

  private findTable(node: abaplint.Nodes.StatementNode, traversal: Traversal): {table: abaplint.Objects.Table | undefined, keys: string[]} {
    let keys: string[] = [];
    let tabl: abaplint.Objects.Table | undefined = undefined;
    const from = node.findAllExpressions(abaplint.Expressions.SQLFromSource).map(e => e.concatTokens());
    if (from.length === 1) {
      tabl = traversal.findTable(from[0]);
      if (tabl) {
        keys = tabl.listKeys(traversal.reg).map(k => k.toLowerCase());
      }
    }
    return {table: tabl, keys};
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