import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler, SQLOrderByTranspiler, SourceTranspiler, SQLCondTranspiler, SQLSourceTranspiler, SQLFieldListTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";
import {SQLFromTranspiler} from "../expressions/sql_from";
import {SQLGroupByTranspiler} from "../expressions/sql_group_by";

function escapeRegExp(string: string) {
  return string.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"); // $& means the whole matched string
}

// TODO: currently SELECT into are always handled as CORRESPONDING
export class SelectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal, targetOverride?: string): Chunk {
    if (node.findDirectTokenByText("UNION") !== undefined) {
      return new Chunk(`throw new Error("SELECT UNION, not supported, transpiler, todo");`);
    } else if (node.findDirectTokenByText("ALL") !== undefined) {
      throw new Error("SelectTranspiler, UNION ALL todo");
    } else if (node.findDirectTokenByText("DISTINCT") !== undefined) {
      throw new Error("SelectTranspiler, UNION DISTINCT todo");
    }

    let target = "undefined";
    let intoCorrespondingStructure = false;
    if (targetOverride) {
      // SelectLoop structure uses override
      target = targetOverride;
    } else if (node.findFirstExpression(abaplint.Expressions.SQLIntoTable)) {
      target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target)).getCode();
    } else if (node.findFirstExpression(abaplint.Expressions.SQLIntoList)) {
      target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.SQLIntoList)).getCode();
    } else if (node.findFirstExpression(abaplint.Expressions.SQLIntoStructure)) {
      const into = node.findFirstExpression(abaplint.Expressions.SQLIntoStructure)!;
      target = traversal.traverse(into).getCode();
      intoCorrespondingStructure = into.findDirectTokenByText("CORRESPONDING") !== undefined;
    }

    const tokens = node.getTokens();
    const isDistinct = tokens[0]?.getStr().toUpperCase() === "SELECT"
      && tokens[1]?.getStr().toUpperCase() === "DISTINCT";
    let select = isDistinct ? "SELECT DISTINCT " : "SELECT ";
    const fieldList = node.findFirstExpression(abaplint.Expressions.SQLFieldList)
      || node.findFirstExpression(abaplint.Expressions.SQLFieldListLoop);
    if (fieldList === undefined) {
      throw new Error("SelectTranspiler, field list not found");
    }
    select += new SQLFieldListTranspiler().transpile(fieldList, traversal).getCode() + " ";

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
    let whereClause = "";
    if (where) {
      whereClause = "WHERE " + new SQLCondTranspiler().transpile(where, traversal, table).getCode() + " ";
    }
    select += whereClause;

    const groupBy = node.findFirstExpression(abaplint.Expressions.SQLGroupBy);
    if (groupBy) {
      select += new SQLGroupByTranspiler().transpile(groupBy, traversal).getCode() + " ";
    }

    const having = node.findDirectExpression(abaplint.Expressions.Select)
      ?.findDirectExpression(abaplint.Expressions.SQLHaving);
    const havingCond = having?.findFirstExpression(abaplint.Expressions.SQLCond);
    if (havingCond) {
      select += "HAVING " + new SQLCondTranspiler().transpile(havingCond, traversal, table).getCode() + " ";
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
      select += new SQLOrderByTranspiler().transpile(orderBy, traversal).getCode();
    }

    const fieldListDynamics = new Set(fieldList.findAllExpressionsRecursive(abaplint.Expressions.Dynamic));
    const groupByDynamics = new Set(groupBy?.findAllExpressionsRecursive(abaplint.Expressions.Dynamic) || []);
    const orderByDynamics = new Set(orderBy?.findAllExpressionsRecursive(abaplint.Expressions.Dynamic) || []);
    for (const d of node.findAllExpressionsRecursive(abaplint.Expressions.Dynamic)) {
      const chain = d.findFirstExpression(abaplint.Expressions.FieldChain);
      if (chain) {
        const search = d.concatTokens();
        if (fieldListDynamics.has(d)) {
          const code = new FieldChainTranspiler(false).transpile(chain, traversal).getCode();
          select = select.replace(search, `" + ${this.dynamicSelectList(code)} + "`);
        } else if (groupByDynamics.has(d)) {
          const code = new FieldChainTranspiler(false).transpile(chain, traversal).getCode();
          select = select.replace("GROUP BY " + search, `" + ${this.dynamicSQLClause("GROUP BY", code)} + "`);
        } else if (orderByDynamics.has(d)) {
          const code = new FieldChainTranspiler(false).transpile(chain, traversal).getCode();
          select = select.replace("ORDER BY " + search, `" + ${this.dynamicSQLClause("ORDER BY", code)} + "`);
        } else {
          const code = new FieldChainTranspiler(true).transpile(chain, traversal).getCode();
          select = select.replace(search, `" + ${code} + "`);
          whereClause = whereClause.replace(search, `" + ${code} + "`);
        }
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
    if (intoCorrespondingStructure) {
      runtimeOptionsList.push(`corresponding: true`);
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
      const unique2 = UniqueIdentifier.get();
      const fn = node.findFirstExpression(abaplint.Expressions.SQLForAllEntries)?.findDirectExpression(abaplint.Expressions.SQLSource);
      const faeTranspiled = new SQLSourceTranspiler().transpile(fn!, traversal).getCode();
      // ABAP semantics: with an empty driving table the whole WHERE condition is ignored
      const selectEmpty = select.replace(whereClause, "");
      const bindRow = (text: string) => text
        .replace(new RegExp(" " + escapeRegExp(faeTranspiled!), "g"), " " + unique)
        .replace(unique + ".get().table_line.get()", unique + ".get()");  // there can be only one?
      select = bindRow(select);
      const where = bindRow(whereClause);

      // FOR ALL ENTRIES removes duplicate rows from the result: duplicates of
      // the selected columns, so de-duplicate by the target's components. The
      // DB key alone is wrong for projections (INTO CORRESPONDING FIELDS
      // without the key fields crashed on the missing component names)
      const by = `Object.keys(${target}.getRowType().get())`;
      const dedup = `if (!(${target} instanceof abap.types.HashedTable) && ${target}.getOptions()?.primaryKey?.type !== "SORTED") {
    abap.statements.sort(${target}, {by: ${by}.map(k => { return {component: k}; })});
    await abap.statements.deleteInternal(${target}, {adjacent: true, allFields: true});
  }`;

      const at = where.startsWith("WHERE ") ? select.indexOf(where) : -1;
      // UP TO with ORDER BY keeps the first n rows in that order; the blocks
      // trim after the de-duplicating sort, so that pair stays row by row
      if (concat.startsWith("SELECT SINGLE ") || at < 0 || (upTo && orderBy)) {
        const code = `if (${faeTranspiled}.array().length === 0) {
  await abap.statements.select(${target}, {select: "${selectEmpty.trim()}"${extra}});
} else {
  const ${unique2} = ${faeTranspiled}.array();
  ${target}.clear();
  for await (const ${unique} of ${unique2}) {
    await abap.statements.select(${target}, {select: "${select.trim()}"${extra}}, {appending: true});
  }
  ${dedup}
  abap.builtin.sy.get().dbcnt.set(${target}.getArrayLength());
}`;
        return new Chunk().append(code, node, traversal);
      }

      // One SELECT per block of driving rows, similar to the kernel's blocking
      // of FOR ALL ENTRIES (rsdb/max_blocking_factor): the condition of each row in
      // parentheses, joined with OR. Not one SELECT per row. UP TO n ROWS
      // counts the whole de-duplicated result, not each block (measured on
      // a 7.5x system: two driving rows and UP TO 3 give 3 rows).
      let tail = select.substring(at + where.length);
      let upToCode = "0";
      if (upTo) {
        const s = upTo.findFirstExpression(abaplint.Expressions.SimpleSource3);
        if (s) {
          const n = new SourceTranspiler(true).transpile(s, traversal).getCode();
          tail = tail.replace(`UP TO " + ${n} + " ROWS `, "");
          upToCode = `parseInt(${n}, 10)`;
        } else {
          tail = tail.replace(upTo.concatTokens() + " ", "");
          upToCode = String(parseInt(upTo.findFirstExpression(abaplint.Expressions.Integer)?.concatTokens() ?? "0", 10));
        }
      }
      const head = select.substring(0, at);
      const condition = where.substring("WHERE ".length).trimEnd();
      const unique3 = UniqueIdentifier.get();
      const unique4 = UniqueIdentifier.get();
      const code = `if (${faeTranspiled}.array().length === 0) {
  await abap.statements.select(${target}, {select: "${selectEmpty.trim()}"${extra}});
} else {
  const ${unique2} = ${faeTranspiled}.array();
  ${target}.clear();
  const ${unique3} = (${unique}) => "(${condition})";
  for (let ${unique4} = 0; ${unique4} < ${unique2}.length; ${unique4} += 50) {
    const ${unique4}where = ${unique2}.slice(${unique4}, ${unique4} + 50).map(${unique3}).join(" OR ");
    await abap.statements.select(${target}, {select: "${head}WHERE " + ${unique4}where + " ${tail.trim()}"${extra}}, {appending: true});
  }
  ${dedup}
  const ${unique4}max = ${upToCode};
  if (${unique4}max > 0 && ${target}.getArrayLength() > ${unique4}max) {
    if (${target} instanceof abap.types.HashedTable) {
      for (const ${unique4}row of ${target}.array().slice(${unique4}max)) {
        await abap.statements.deleteInternal(${target}, {fromValue: ${unique4}row});
      }
    } else {
      while (${target}.getArrayLength() > ${unique4}max) {
        ${target}.deleteIndex(${target}.getArrayLength() - 1);
      }
    }
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

  private dynamicSelectList(code: string): string {
    return `(${code} instanceof abap.types.Table || ${code} instanceof abap.types.HashedTable`
      + ` ? (${code}.array().length === 0 ? "*" : ${code}.array().map(row => row.get()).join(", "))`
      + ` : ${code}.get())`;
  }

  private dynamicSQLClause(keyword: string, code: string): string {
    return `(${code} instanceof abap.types.Table || ${code} instanceof abap.types.HashedTable`
      + ` ? (${code}.array().length === 0 ? "" : "${keyword} " + ${code}.array().map(row => row.get()).join(", "))`
      + ` : (("" + ${code}.get()).trim() === "" ? "" : "${keyword} " + ${code}.get()))`;
  }

  private isWhereExpression(node: abaplint.Nodes.StatementNode, expression: abaplint.Nodes.ExpressionNode): boolean {
    // check if previous token before sqlCond is "WHERE". It could also be "ON" in case of join condition
    let prevToken;
    const sqlCondToken = expression.getFirstToken();
    for (const token of node.getTokens()) {
      if (token.getStart() === sqlCondToken.getStart()) {
        break;
      }
      prevToken = token;
    }
    if (prevToken && prevToken.getStr().toUpperCase() === "WHERE") {
      return true;
    } else {
      return false;
    }
  }

}
