import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DatabaseTableTranspiler, SQLFieldAndValueTranspiler} from "../expressions";

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

    return new Chunk(`await abap.statements.updateDatabase(${table.getCode()}, {${options.join(", ")}});`);
  }

}