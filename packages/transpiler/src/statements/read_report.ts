import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";
import {SourceTranspiler, TargetTranspiler} from "../expressions/index.js";

export class ReadReportTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const reportNode = node.findExpressionAfterToken("REPORT");
    const reportChunk = new SourceTranspiler().transpile(reportNode!, traversal);

    const options: string[] = [];
    const intoNode = node.findExpressionAfterToken("INTO");
    if (intoNode) {
      options.push("into: " + new TargetTranspiler().transpile(intoNode, traversal).getCode());
    }

    const stateNode = node.findExpressionAfterToken("STATE");
    if (stateNode) {
      options.push("state: " + new SourceTranspiler().transpile(stateNode, traversal).getCode());
    }

    return new Chunk().appendString(`abap.statements.readReport(`)
      .appendChunk(reportChunk)
      .appendString(", {" + options.join(",") + "}")
      .appendString(");");
  }

}