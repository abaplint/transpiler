import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ScanTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const lookup = traversal.lookupClassOrInterface("KERNEL_SCAN_ABAP_SOURCE", node.getFirstToken());
    const options: string[] = [];

    let name = "";
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.ExpressionNode) {
        options.push(name + ": " + traversal.traverse(c).getCode());
        name = "";
      } else {
        if (name !== "" && name.endsWith("_") === false) {
          name += "_";
        }
        name += c.concatTokens().toLowerCase().replace("-", "");
      }
    }

    const call = `await ${lookup}.call({${options.join(", ")}});`;
    return new Chunk().append(
      `if (${lookup} === undefined) throw new Error("ScanAbapSource, kernel class missing");\n${call}`, node, traversal);
  }

}