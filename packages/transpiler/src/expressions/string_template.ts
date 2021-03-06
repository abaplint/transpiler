import {Nodes, Tokens, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler} from ".";
import {Traversal} from "../traversal";

export class StringTemplateTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";
    const children = node.getChildren();
    for (let i = 0; i < children.length; i++) {
      const c = children[i];
      const g = c.get();
      if (c instanceof Nodes.TokenNode) {
        let original = c.getFirstToken().getStr();
        original = original.substring(1, original.length - 1);
        original = original.replace(/`/g, "\\`");
        if (g instanceof Tokens.StringTemplate) {
          ret = "`" + original + "`";
        } else if (g instanceof Tokens.StringTemplateBegin) {
          ret = "`" + original + "${";
        } else if (g instanceof Tokens.StringTemplateMiddle) {
          ret = ret + "}" + original + "${";
        } else if (g instanceof Tokens.StringTemplateEnd) {
          ret = ret + "}" + original + "`";
        }
      } else if (c instanceof Nodes.ExpressionNode) {
        const next = children[i + 1];
        let pre = "";
        let post = "";
        let get = true;
        if (next instanceof Nodes.ExpressionNode && next.get() instanceof Expressions.StringTemplateFormatting) {
          const options = this.build(next);
          if (options) {
            pre = "abap.templateFormatting(";
            post = "," + options + ")";
            get = false;
          }
        }
        if (g instanceof Expressions.Source) {
          ret = ret + pre + new SourceTranspiler(get).transpile(c, traversal) + post;
        }
      }
    }

    return ret;
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