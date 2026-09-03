import {Expressions, Nodes, Tokens} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FieldChainTranspiler} from "./field_chain";

export class FieldOffsetTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = "";
    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        if (c.get() instanceof Expressions.SimpleFieldChain2) {
          ret = new FieldChainTranspiler().transpile(c, traversal).getCode();
        }
      } else if(c instanceof Nodes.TokenNode) {
        if (c.get() instanceof Tokens.Identifier) {
          let value = c.getFirstToken().getStr().toLowerCase();
          if (/^0+\d+$/.test(value)) {
            value = value.replace(/^0+/, "") || "0";
          }
          ret += value;
        }
      }
    }
    return new Chunk(ret);
  }

}
