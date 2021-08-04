import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class CompareOperatorTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): Chunk {
    const op = node.getFirstToken().getStr().toUpperCase();
    switch(op) {
      case "=":
      case "EQ":
        return new Chunk("eq");
      case "<":
      case "LT":
        return new Chunk("lt");
      case "<=":
      case "LE":
        return new Chunk("le");
      case ">":
      case "GT":
        return new Chunk("gt");
      case ">=":
      case "GE":
        return new Chunk("ge");
      case "<>":
      case "NE":
        return new Chunk("ne");
      case "CO":
        return new Chunk("co");
      case "CP":
        return new Chunk("cp");
      case "CA":
        return new Chunk("ca");
      case "CS":
        return new Chunk("cs");
      case "NS":
        return new Chunk("ns");
      default:
        return new Chunk("compareoperatortodo" + op);
    }
  }

}