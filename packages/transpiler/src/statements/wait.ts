import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";
import {SourceTranspiler} from "../expressions/index.js";

export class WaitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const options: string[] = [];

    const seconds = node.findExpressionAfterToken("TO");
    if (seconds) {
      options.push("seconds: " + traversal.traverse(seconds).getCode());
    }

    const concat = node.concatTokens().toUpperCase();
    if (concat.includes(" UNTIL ") === false && seconds) {
      const sec = new SourceTranspiler(true).transpile(seconds, traversal).getCode();
      return new Chunk().appendString(`await new Promise(r => setTimeout(r, ${sec} * 1000));`);
    }

    const lookup = traversal.lookupClassOrInterface("KERNEL_PUSH_CHANNELS", node.getFirstToken());

    const cond = node.findFirstExpression(abaplint.Expressions.Cond);
    if (cond) {
      options.push("cond: " + traversal.traverse(cond).getCode());
    }

    const call = `await ${lookup}.wait({${options.join(",")}});`;

    return new Chunk().append(
      `if (${lookup} === undefined) throw new Error("Wait, kernel class missing");\n${call}`, node, traversal);
  }

}