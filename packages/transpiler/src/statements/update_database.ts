import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DatabaseTableTranspiler, SourceTranspiler, SQLFieldAndValueTranspiler} from "../expressions";

export class UpdateDatabaseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const dbtab = node.findFirstExpression(abaplint.Expressions.DatabaseTable);
    if (dbtab === undefined) {
      throw new Error("internal error, UpdateDatabaseTranspiler");
    }
    const table = new DatabaseTableTranspiler(false).transpile(dbtab, traversal);

    const options: string[] = [];

    const cond = node.findFirstExpression(abaplint.Expressions.SQLCond);
    if (cond) {
      const ttab = traversal.traverse(cond);
      options.push(`"where": "` + ttab.getCode() + `"`);
    }

    const sets = node.findAllExpressions(abaplint.Expressions.SQLFieldAndValue);
    if (sets.length > 0) {
      const s = [];
      for (const set of sets) {
        s.push(new SQLFieldAndValueTranspiler().transpile(set, traversal).getCode());
      }
      options.push(`"set": [${s.join(",")}]`);
    }

    const s = node.findDirectExpression(abaplint.Expressions.SQLSource)!.findDirectExpression(abaplint.Expressions.SimpleSource3)!;
    const sqlSource = new SourceTranspiler(true).transpile(s, traversal).getCode();

    if (sqlSource) {
      const tableName = node.findDirectExpression(abaplint.Expressions.DatabaseTable)?.concatTokens();
      const tabl = traversal.findTable(tableName!)!;
      const keys = tabl.listKeys(traversal.reg).map(k => k.toLowerCase());
      const allFields = (tabl.parseType(traversal.reg) as abaplint.BasicTypes.StructureType).getComponents().map(c => {
        return c.name.toLowerCase();
      });

      const where: string[] = [];
      const set: string[] = [];
      for (const fieldName of allFields) {
        const cond = `\\"${fieldName}\\" = '\" + ${sqlSource}.${fieldName}.get() + \"'`;
        if (keys.includes(fieldName) === true) {
          where.push(cond);
        }  else {
          set.push("\"" + cond + "\"");
        }
      }

      options.push(`"where": "` + where.join(" AND ") + `"`);
      options.push(`"set": [${set.join(",")}]`);
    }

    return new Chunk(`await abap.statements.updateDatabase(${table.getCode()}, {${options.join(", ")}});`);
  }

}