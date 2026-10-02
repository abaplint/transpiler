import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DatasetOperands} from "./_dataset";

export class ReadDatasetTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const operands = new DatasetOperands(node, traversal);
    const options: string[] = [];
    if (operands.after("MAXIMUM LENGTH") !== undefined) {
      options.push("maximumLength: " + operands.after("MAXIMUM LENGTH"));
    }
    // the obsolete LENGTH is ACTUAL LENGTH
    const actual = operands.after("ACTUAL LENGTH") ?? operands.after("LENGTH", ["MAXIMUM", "ACTUAL"]);
    if (actual !== undefined) {
      options.push("actualLength: " + actual);
    }
    return new Chunk()
      .append("await abap.statements.readDataset(", node, traversal)
      .appendString(operands.first() + ", " + operands.after("INTO") + ", {" + options.join(", ") + "}")
      .append(");", node.getLastToken(), traversal);
  }

}
