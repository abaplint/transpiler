import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class ModifyDatabaseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const table = traversal.traverse(node.findFirstExpression(abaplint.Expressions.DatabaseTable));

    const options: string[] = [];

    const tab = node.findExpressionAfterToken("TABLE");
    if (tab) {
      const ttab = traversal.traverse(tab);
      options.push(`"table": ` + ttab.getCode());
    }

    const from = node.findExpressionAfterToken("FROM");
    if (from && from.get() instanceof abaplint.Expressions.SQLSource) {
      const tvalues = traversal.traverse(from);
      options.push(`"values": ` + tvalues.getCode());
    }

    return new Chunk(`await abap.statements.modifyDatabase(${table.getCode()}, {${options.join(", ")}});`);
  }

}