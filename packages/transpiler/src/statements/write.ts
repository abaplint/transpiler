import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class WriteTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const extra: string[] = [];
    let source: Chunk | undefined;

    const newLine = node.findFirstExpression(abaplint.Expressions.WriteOffsetLength)?.findDirectTokenByText("/") !== undefined;
    const concat = node.concatTokens().toUpperCase();

    const target = node.findDirectExpression(abaplint.Expressions.Target);
    if (target) {
      extra.push("target: " + traversal.traverse(target).getCode());
    }

    const exponent = node.findExpressionAfterToken("EXPONENT");
    if (exponent) {
      extra.push("exponent: " + traversal.traverse(exponent).getCode());
    }

    const currency = node.findExpressionAfterToken("CURRENCY");
    if (currency) {
      extra.push("currency: " + traversal.traverse(currency).getCode());
    }

    if (concat.includes("NO-GROUPING")) {
      extra.push("noGrouping: true");
    }

    if (concat.includes("NO-SIGN")) {
      extra.push("noSign: true");
    }

    const expr = node.findDirectExpression(abaplint.Expressions.Source);
    if (expr === undefined) {
      source = new Chunk().append("''", node, traversal);
      extra.push("newLine: true");
      extra.push("skipLine: true");
    } else {
      if (newLine === true) {
        extra.push("newLine: true");
      }
      const concat = expr.concatTokens();
      if (concat.startsWith("'@KERNEL ")) {
        // @KERNEL commands must be taken verbatim
        return new Chunk().append(concat.substr(9, concat.length - 10), node, traversal);
      }
      source = traversal.traverse(expr);
    }

    const chunk = new Chunk();
    chunk.append("abap.statements.write(", node, traversal);
    chunk.appendChunk(source);
    if (extra.length === 0) {
      chunk.append(");", node.getLastToken(), traversal);
    } else {
      chunk.append(",{" + extra.join(",") + "});", node.getLastToken(), traversal);
    }
    return chunk;
  }

}