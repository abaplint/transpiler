import {Nodes} from "abaplint";

export interface IStatementTranspiler {
  transpile(node: Nodes.StatementNode): string;
}