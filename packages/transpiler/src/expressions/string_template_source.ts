import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler} from ".";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class StringTemplateSourceTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = "";

    let pre = "";
    let post = "";
    let get = true;
    const next = node.findDirectExpression(Expressions.StringTemplateFormatting);
    if (next) {
      const options = this.build(next);
      if (options) {
        pre = "abap.templateFormatting(";
        post = "," + options + ")";
        get = false;
      }
    }

    const c = node.findDirectExpression(Expressions.Source);
    if (c === undefined) {
      throw new Error("StringTemplateSourceTranspiler, Source not found");
    }
    ret += pre + new SourceTranspiler(get).transpile(c, traversal).getCode() + post;

    return new Chunk(ret);
  }

  private build(node: Nodes.ExpressionNode): undefined | string {
    const concat = node.concatTokens().toUpperCase();
    if (concat === "TIMESTAMP = ISO") {
      return "{timestamp: 'iso'}";
    } else if (concat === "DATE = ISO") {
      return "{date: 'iso'}";
    } else if (concat === "TIME = ISO") {
      return "{time: 'iso'}";
    }
    return undefined;
  }

}