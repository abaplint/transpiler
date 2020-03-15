import {Nodes, SpaghettiScope} from "abaplint";

export interface IExpressionTranspiler {
  transpile(node: Nodes.ExpressionNode, spaghetti: SpaghettiScope, filename: string): string;
}