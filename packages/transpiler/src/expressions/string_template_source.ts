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
      const options = this.build(next, traversal);
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

  private build(node: Nodes.ExpressionNode, traversal: Traversal): undefined | string {
    let option = "";
    let count = 0;
    for (const c of node.getChildren()) {
      count++;
      if (c instanceof Nodes.TokenNode) {
        if (c.getFirstToken().getStr() === "=") {
          option += ":";
        } else {
          if (count > 3 && (count - 1) % 3 === 0) {
            option += ",";
          }
          option += `"` + c.concatTokens().toLowerCase() + `"`;
        }
      } else if (c.get() instanceof Expressions.Source) {
        option += new SourceTranspiler(true).transpile(c, traversal).getCode();
      }
    }
    if (option !== "") {
      return "{" + option + "}";
    }
    return undefined;
  }

}