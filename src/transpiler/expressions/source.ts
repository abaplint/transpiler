import {Expressions, Nodes} from "abaplint";

export function translateSource(node: Nodes.ExpressionNode): string {
  const int = node.findFirstExpression(Expressions.Integer)!.getFirstToken().getStr();
  return int;
}