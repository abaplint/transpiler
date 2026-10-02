import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DatasetOperands} from "./_dataset";

export class SetDatasetTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const operands = new DatasetOperands(node, traversal);
    const option = operands.after("POSITION") !== undefined
      ? "position: " + operands.after("POSITION")
      : "endOfFile: true";
    return new Chunk()
      .append("await abap.statements.setDataset(", node, traversal)
      .appendString(operands.first() + ", {" + option + "}")
      .append(");", node.getLastToken(), traversal);
  }

}
