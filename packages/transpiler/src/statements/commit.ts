import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {findConnection} from "./insert_database";

export class CommitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const options: string[] = [];

    const connection = node.findDirectExpression(abaplint.Expressions.DatabaseConnection);
    if (connection) {
      options.push(`"connection": "${findConnection(connection)}"`);
    } else if (node.findTokenSequencePosition("AND", "WAIT")) {
      options.push(`"wait": true`);
    }

    const opt = options.length === 0 ? "" : `{${options.join(", ")}}`;
    return new Chunk().append(`await abap.statements.commit(${opt});`, node, traversal);
  }

}
