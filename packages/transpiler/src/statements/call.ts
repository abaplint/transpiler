import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {MethodSourceTranspiler} from "../expressions";

export class CallTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    if (node.concatTokens().toLowerCase().includes("super->constructor")) {
      // todo, https://github.com/abaplint/transpiler/issues/133
      return new Chunk("");
    }

    const chain = node.findDirectExpression(abaplint.Expressions.MethodCallChain);
    if (chain) {
      return new Chunk()
        .appendChunk(traversal.traverse(chain))
        .append(";", node.getLastToken(), traversal);
    }

    const methodSource = node.findDirectExpression(abaplint.Expressions.MethodSource);
    if (methodSource) {
      let body = "";
      const methodCallBody = node.findDirectExpression(abaplint.Expressions.MethodCallBody);
      if (methodCallBody && 1 === 1 + 2) { // todo
        body += traversal.traverse(methodCallBody).getCode();
      }

      let pre = "";
      let post = "";
      const receiving = node.findFirstExpression(abaplint.Expressions.MethodParameters)?.findExpressionAfterToken("RECEIVING");
      if (receiving) {
        const target = traversal.traverse(receiving.findDirectExpression(abaplint.Expressions.Target));
        pre = target.getCode() + ".set(";
        post = ")";
      }

      const ms = new MethodSourceTranspiler(pre).transpile(methodSource, traversal);

      return new Chunk().appendChunk(ms).appendString("(" + body + ")" + post + ";");
    }

    throw new Error("CallTranspiler, todo");
  }

}