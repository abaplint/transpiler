import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class CompareOperatorTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const op = node.getFirstToken().getStr().toUpperCase();
    switch(op) {
      case "=":
      case "EQ":
        return new Chunk().append("eq", node, traversal);
      case "<":
      case "LT":
        return new Chunk().append("lt", node, traversal);
      case "<=":
      case "LE":
        return new Chunk().append("le", node, traversal);
      case ">":
      case "GT":
        return new Chunk().append("gt", node, traversal);
      case ">=":
      case "GE":
        return new Chunk().append("ge", node, traversal);
      case "<>":
      case "NE":
        return new Chunk().append("ne", node, traversal);
      case "CO":
        return new Chunk().append("co", node, traversal);
      case "CP":
        return new Chunk().append("cp", node, traversal);
      case "CA":
        return new Chunk().append("ca", node, traversal);
      case "CS":
        return new Chunk().append("cs", node, traversal);
      case "NS":
        return new Chunk().append("ns", node, traversal);
      case "CN":
        return new Chunk().append("cn", node, traversal);
      case "NA":
        return new Chunk().append("na", node, traversal);
      default:
        return new Chunk().append("compareoperatortodo" + op, node, traversal);
    }
  }

}