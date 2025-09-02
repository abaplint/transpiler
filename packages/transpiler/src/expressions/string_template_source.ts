import {Nodes, Expressions, AbstractType} from "@abaplint/core";
import {SourceTranspiler} from ".";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";

export class StringTemplateSourceTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, context?: AbstractType): Chunk {
    let ret = "";

    const pre = "abap.templateFormatting(";
    let post = ")";
    const formatting = node.findDirectExpression(Expressions.StringTemplateFormatting);
    if (formatting) {
      const options = this.build(formatting, traversal, context);
      if (options) {
        post = "," + options + ")";
      }
    }

    const c = node.findDirectExpression(Expressions.Source);
    if (c === undefined) {
      throw new Error("StringTemplateSourceTranspiler, Source not found");
    }
    ret += pre + new SourceTranspiler().transpile(c, traversal).getCode() + post;

    return new Chunk(ret);
  }

  private build(node: Nodes.ExpressionNode, traversal: Traversal, context?: AbstractType): undefined | string {
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
    if (option.startsWith(`"alpha"`)) {
      if (context === undefined) {
        throw new Error("ALPHA = IN, context undefined");
      }
      option += `, "alphaInContext": ` + TranspileTypes.toType(context);
    }
    if (option !== "") {
      return "{" + option + "}";
    }
    return undefined;
  }

}