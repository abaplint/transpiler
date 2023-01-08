import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class UpdateDatabaseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const table = traversal.traverse(node.findFirstExpression(abaplint.Expressions.DatabaseTable));

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
        const name = set.findDirectExpression(abaplint.Expressions.SQLFieldName)?.concatTokens();
        const source = traversal.traverse(set.findDirectExpression(abaplint.Expressions.SQLSource));
        s.push("\"'" + name + "' = '\" + " + source.getCode() + ".get() + \"'\"");
      }
      options.push(`"set": [${s.join(",")}]`);
    }

    return new Chunk(`await abap.statements.updateDatabase(${table.getCode()}, {${options.join(", ")}});`);
  }

}