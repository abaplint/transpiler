import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DatasetOperands} from "./_dataset";

export class OpenDatasetTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const operands = new DatasetOperands(node, traversal);
    const options: string[] = [];

    const mode = ["INPUT", "OUTPUT", "APPENDING", "UPDATE"].find(m => operands.has("FOR " + m)) ?? "INPUT";
    options.push(`mode: "${mode}"`);
    options.push(`binary: ${operands.has("BINARY MODE")}`);
    const encoding = ["DEFAULT", "UTF-8", "NON-UNICODE"].find(e => operands.has("ENCODING " + e));
    if (encoding !== undefined) {
      options.push(`encoding: "${encoding}"`);
    }
    if (operands.has("IN LEGACY")) {
      options.push("legacy: true");
    }
    if (operands.after("MESSAGE") !== undefined) {
      options.push("message: " + operands.after("MESSAGE"));
    }
    if (operands.after("AT POSITION") !== undefined) {
      options.push("position: " + operands.after("AT POSITION"));
    }
    // accepted by the syntax, not implemented by the runtime: it refuses them
    // by name rather than ignoring them
    const unsupported = ["CODE PAGE", "TYPE", "FILTER", "REPLACEMENT CHARACTER", "WITH BYTE-ORDER MARK", "SKIPPING BYTE-ORDER MARK",
      "WITH SMART LINEFEED", "WITH NATIVE LINEFEED", "WITH UNIX LINEFEED", "WITH WINDOWS LINEFEED", "IGNORING CONVERSION ERRORS"]
      .filter(u => operands.has(u) || operands.after(u) !== undefined);
    if (unsupported.length > 0) {
      options.push("unsupported: " + JSON.stringify(unsupported));
    }

    return new Chunk()
      .append("await abap.statements.openDataset(", node, traversal)
      .appendString(operands.first() + ", {" + options.join(", ") + "}")
      .append(");", node.getLastToken(), traversal);
  }

}
