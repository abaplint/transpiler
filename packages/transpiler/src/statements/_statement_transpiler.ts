import {Nodes, SpaghettiScope} from "abaplint";

export interface IStatementTranspiler {
  transpile(node: Nodes.StatementNode, spaghetti: SpaghettiScope, filename: string): string;
}