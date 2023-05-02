import {Expressions, Nodes, Tokens} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";
import {FieldChainTranspiler} from "./field_chain.js";

export class FieldLengthTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = "";
    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        if (c.get() instanceof Expressions.SimpleFieldChain2) {
          ret = new FieldChainTranspiler().transpile(c, traversal).getCode();
        }
      } else if(c instanceof Nodes.TokenNode) {
        if (c.get() instanceof Tokens.Identifier) {
          ret += c.getFirstToken().getStr();
        }
      }
    }
    return new Chunk(ret);
  }

}