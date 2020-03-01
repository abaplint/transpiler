import {Nodes} from "abaplint";

export interface IExpressionTranspiler {
  transpile(node: Nodes.ExpressionNode): string;
}