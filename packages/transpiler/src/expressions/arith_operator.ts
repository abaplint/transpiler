import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {Traversal} from "../traversal.js";
import {IExpressionTranspiler} from "./_expression_transpiler.js";

export class ArithOperatorTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let search = node.concatTokens().toUpperCase();
    // macro workaround,
    search = search.replace(/ /g, "");
    switch(search) {
      case "+":
        return new Chunk().append("abap.operators.add", node, traversal);
      case "-":
        return new Chunk().append("abap.operators.minus", node, traversal);
      case "*":
        return new Chunk().append("abap.operators.multiply", node, traversal);
      case "/":
        return new Chunk().append("abap.operators.divide", node, traversal);
      case "**":
        return new Chunk().append("abap.operators.power", node, traversal);
      case "DIV":
        return new Chunk().append("abap.operators.div", node, traversal);
      case "MOD":
        return new Chunk().append("abap.operators.mod", node, traversal);
      case "BIT-AND":
        return new Chunk().append("abap.operators.bitand", node, traversal);
      case "BIT-OR":
        return new Chunk().append("abap.operators.bitor", node, traversal);
      case "BIT-XOR":
        return new Chunk().append("abap.operators.bitxor", node, traversal);
      default:
        console.dir(search);
        return new Chunk().append(".ArithOperatorUnknown", node, traversal);
    }
  }

}