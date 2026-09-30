import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DatasetOperands} from "./_dataset";

export class GetDatasetTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const operands = new DatasetOperands(node, traversal);
    const options: string[] = [];
    if (operands.after("POSITION") !== undefined) {
      options.push("position: " + operands.after("POSITION"));
    }
    if (operands.after("ATTRIBUTES") !== undefined) {
      options.push("attributes: " + operands.after("ATTRIBUTES"));
    }
    return new Chunk()
      .append("await abap.statements.getDataset(", node, traversal)
      .appendString(operands.first() + ", {" + options.join(", ") + "}")
      .append(");", node.getLastToken(), traversal);
  }

}
