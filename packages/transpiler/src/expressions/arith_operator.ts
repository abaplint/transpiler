import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ArithOperatorTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): Chunk {
    switch(node.concatTokens()) {
      case "+":
        return new Chunk("abap.operators.add");
      case "-":
        return new Chunk("abap.operators.minus");
      case "*":
        return new Chunk("abap.operators.multiply");
      case "/":
        return new Chunk("abap.operators.divide");
      case "**":
        return new Chunk("abap.operators.power");
      case "DIV":
        return new Chunk("abap.operators.div");
      case "MOD":
        return new Chunk("abap.operators.mod");
      case "BIT-AND":
        return new Chunk("abap.operators.bitand");
      case "BIT-OR":
        return new Chunk("abap.operators.bitor");
      case "BIT-XOR":
        return new Chunk("abap.operators.bitxor");
      default:
        return new Chunk(".ArithOperatorUnknown");
    }
  }

}