import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {MethodCallChainTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class CallTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const chain = node.findFirstExpression(abaplint.Expressions.MethodCallChain);
    if (chain) {
      return new MethodCallChainTranspiler().transpile(chain, traversal) + ";";
    }

    throw new Error("CallTranspiler, todo");
  }

}