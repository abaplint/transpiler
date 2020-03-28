import {Nodes, Tokens, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler} from ".";
import {Traversal} from "../traversal";

export class StringTemplateTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {

    let ret = "";

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.TokenNode) {
        let original = c.getFirstToken().getStr();
        original = original.substring(1, original.length - 1);
        if (c.get() instanceof Tokens.StringTemplate) {
          ret = "`" + original + "`";
        } else if (c.get() instanceof Tokens.StringTemplateBegin) {
          ret = "`" + original + "${";
        } else if (c.get() instanceof Tokens.StringTemplateMiddle) {
          ret = ret + "}" + original + "${";
        } else if (c.get() instanceof Tokens.StringTemplateEnd) {
          ret = ret + "}" + original + "`";
        }
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Source) {
        ret = ret + new SourceTranspiler(true).transpile(c, traversal);
      }
    }

    return ret;
  }

}