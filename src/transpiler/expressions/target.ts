import {Expressions, Nodes} from "abaplint";

export function translateTarget(node: Nodes.ExpressionNode): string {
  const name = node.findFirstExpression(Expressions.TargetField)!.getFirstToken().getStr();
  return name;
}