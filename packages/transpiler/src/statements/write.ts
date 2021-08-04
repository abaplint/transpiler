import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class WriteTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    let extra = "";
    let source: Chunk | undefined;
    const newLine = node.findFirstExpression(abaplint.Expressions.WriteOffsetLength)?.findDirectTokenByText("/") !== undefined;

    const expr = node.findDirectExpression(abaplint.Expressions.Source);
    if (expr === undefined) {
      source = new Chunk().appendString("''");
      extra = ", {newLine: true,skipLine: true}";
    } else {
      if (newLine === true) {
        extra = ", {newLine: true}";
      }
      const concat = expr.concatTokens();
      if (concat.startsWith("'@KERNEL ")) {
        // @KERNEL commands must be taken verbatim
        return new Chunk(concat.substr(9, concat.length - 10));
      }
      source = new SourceTranspiler().transpile(expr, traversal);
    }

    const chunk = new Chunk();
    chunk.append("abap.statements.write(", node, traversal);
    chunk.appendChunk(source);
    chunk.appendString(extra + ");");
    return chunk;
  }

}