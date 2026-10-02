import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class DeleteDatasetTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const name = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source)).getCode();
    return new Chunk()
      .append("await abap.statements.deleteDataset(", node, traversal)
      .appendString(name)
      .append(");", node.getLastToken(), traversal);
  }

}
