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
      let pre = "";
      let post = "";
      const receiving = chain.findFirstExpression(abaplint.Expressions.MethodParameters
      )?.findDirectExpression(abaplint.Expressions.ParameterT
      )?.findDirectExpression(abaplint.Expressions.Target);
      if (receiving) {
        pre = traversal.traverse(receiving).getCode() + ".set(";
        post = ")";
      }

      const exceptions = node.findFirstExpression(abaplint.Expressions.ParameterListExceptions);
      if (exceptions) {
        pre = "try {\n" + pre;
      }

      post += ";";

      if (exceptions) {
        post += `\nabap.builtin.sy.get().subrc.set(0);
} catch (e) {
if (e.classic) {
  switch (e.classic.toUpperCase()) {\n`;
        for (const e of exceptions.findAllExpressions(abaplint.Expressions.ParameterException)) {
          const name = e.getFirstToken().getStr().toUpperCase();
          const value = e.findFirstExpression(abaplint.Expressions.SimpleName)?.getFirstToken().getStr().toUpperCase();
          if (value === undefined) {
            continue;
          }
          if (name === "OTHERS") {
            post += `default: abap.builtin.sy.get().subrc.set(${value}); break;\n`;
          } else {
            post += `case "${name}": abap.builtin.sy.get().subrc.set(${value}); break;\n`;
          }
        }
        post += `  }
} else {
  throw e;
}
}`;
      }

      return new Chunk()
        .appendString(pre)
        .appendChunk(traversal.traverse(chain))
        .append(post, node.getLastToken(), traversal);
    }

    const methodSource = node.findDirectExpression(abaplint.Expressions.MethodSource);
    if (methodSource) {
      let body = "";
      const methodCallBody = node.findDirectExpression(abaplint.Expressions.MethodCallBody);
      if (methodCallBody) {
        body = traversal.traverse(methodCallBody).getCode();
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