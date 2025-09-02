import {Nodes, Tokens, AbstractType} from "@abaplint/core";
import {StringTemplateSourceTranspiler} from ".";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class StringTemplateTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, context?: AbstractType): Chunk {
    let ret = "";
    const children = node.getChildren();
    // eslint-disable-next-line @typescript-eslint/prefer-for-of
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
        ret += new StringTemplateSourceTranspiler().transpile(c, traversal, context).getCode();
      }
    }

    return new Chunk("new abap.types.String().set(" + ret + ")");
  }

}