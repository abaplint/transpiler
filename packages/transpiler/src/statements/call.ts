import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {MethodCallBodyTranspiler, MethodSourceTranspiler} from "../expressions";

export class CallTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

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

      post += ";";

      const exceptions = node.findFirstExpression(abaplint.Expressions.ParameterListExceptions);
      if (exceptions) {
        const build = this.buildExceptions(exceptions);
        pre = build.pre + pre;
        post += build.post;
      }

      const chainChunk = traversal.traverse(chain);
      let chainCode = chainChunk.getCode();
      if (chainCode.startsWith("await super.constructor(")) {
// semantics of constructors in JS vs ABAP is different, so the "constructor_" has been introduced,
        chainCode = chainCode.replace("await super.constructor(", "await super.constructor_(");
      }

      return new Chunk()
        .appendString(pre)
        .appendString(chainCode)
        .append(post, node.getLastToken(), traversal);
    }

    const methodSource = node.findDirectExpression(abaplint.Expressions.MethodSource);
    if (methodSource) {
      let body = "";

      const nameToken = methodSource.getLastChild()?.getFirstToken();
      const m = nameToken ? traversal.findMethodReference(nameToken, traversal.findCurrentScopeByToken(nameToken)) : undefined;

      const methodCallBody = node.findDirectExpression(abaplint.Expressions.MethodCallBody);
      if (methodCallBody) {
        body = new MethodCallBodyTranspiler(m?.def).transpile(methodCallBody, traversal).getCode();
      }

      let pre = "";
      let post = "";

      const receiving = node.findFirstExpression(abaplint.Expressions.MethodParameters)?.findExpressionAfterToken("RECEIVING");
      if (receiving) {
        const target = traversal.traverse(receiving.findDirectExpression(abaplint.Expressions.Target));
        pre = target.getCode() + ".set(";
        post = ")";
      }

      const exceptions = node.findFirstExpression(abaplint.Expressions.ParameterListExceptions);
      if (exceptions) {
        const build = this.buildExceptions(exceptions);
        pre = build.pre + pre;
        post += build.post;
      }

      let ms = new MethodSourceTranspiler(pre).transpile(methodSource, traversal).getCode();
      if (ms === "await super.get().constructor") {
// semantics of constructors in JS vs ABAP is different, so the "constructor_" has been introduced,
        ms = "await super.constructor_";
      } else if (ms.startsWith("await super.get()")) {
        ms = ms.replace("await super.get()", "await super");
      }

      return new Chunk().appendString(ms).appendString("(" + body + ")" + post + ";");
    }

    throw new Error("CallTranspiler, todo");
  }

  private buildExceptions(node: abaplint.Nodes.ExpressionNode) {
    let pre = "";
    let post = "";

    pre = "try {\n" + pre;

    post += `\nabap.builtin.sy.get().subrc.set(0);
} catch (e) {
if (e.classic) {
  switch (e.classic.toUpperCase()) {\n`;
    for (const e of node.findAllExpressions(abaplint.Expressions.ParameterException)) {
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

    return {pre, post};
  }

}