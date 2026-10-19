import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {AttributeNameTranspiler} from "./attribute_name";

export class AttributeChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    let arrow = "";

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.AttributeName) {
        ret.appendChunk(traversal.traverse(c));
      } else if (c.get() instanceof Expressions.ArrowOrDash) {
        arrow = c.concatTokens();
        if (arrow === "-" || arrow === "->") {
          ret.append(".get()", c, traversal);
        }
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "*") {
        ret.append(".dereference()", c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ComponentName) {
        if (arrow === "-") {
          // structure component
          const name = c.getFirstToken().getStr().toLowerCase();
          if (Traversal.isBracketComponent(name)) {
            ret.append(`["` + name + `"]`, c, traversal);
          } else {
            ret.append("." + name, c, traversal);
          }
        } else {
          // attribute of an object, eg. "node->get_first_token( )->start->row"
          ret.appendString(".").appendChunk(new AttributeNameTranspiler().transpile(c, traversal));
        }
      } else {
        ret.append("AttributeChainTodo", node, traversal);
      }
    }

    return ret;
  }

}
