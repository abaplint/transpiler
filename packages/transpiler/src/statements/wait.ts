import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class WaitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const concat = node.concatTokens().toUpperCase();
    if (concat.includes(" FOR PUSH CHANNELS ") === false) {
      return new Chunk().appendString("WAIT_TODO");
    }

    const lookup = traversal.lookupClassOrInterface("KERNEL_PUSH_CHANNELS", node.getFirstToken());

    const options: string[] = [];

    const cond = node.findFirstExpression(abaplint.Expressions.Cond);
    if (cond) {
      options.push("cond: " + traversal.traverse(cond).getCode());
    }

    const seconds = node.findExpressionAfterToken("TO");
    if (seconds) {
      options.push("seconds: " + traversal.traverse(seconds).getCode());
    }

    const call = `await ${lookup}.wait({${options.join(",")}});`;

    return new Chunk().append(
      `if (${lookup} === undefined) throw new Error("Wait, kernel class missing");\n${call}`, node, traversal);
  }

}