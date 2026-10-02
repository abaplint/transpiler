import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DatasetOperands} from "./_dataset";

export class TransferTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const operands = new DatasetOperands(node, traversal);
    const options: string[] = [];
    if (operands.after("LENGTH", ["MAXIMUM", "ACTUAL"]) !== undefined) {
      options.push("length: " + operands.after("LENGTH", ["MAXIMUM", "ACTUAL"]));
    }
    if (operands.has("NO END OF LINE")) {
      options.push("noEndOfLine: true");
    }
    return new Chunk()
      .append("await abap.statements.transfer(", node, traversal)
      .appendString(operands.first() + ", " + operands.after("TO") + ", {" + options.join(", ") + "}")
      .append(");", node.getLastToken(), traversal);
  }

}
