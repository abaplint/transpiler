import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {MethodCallChainTranspiler} from "../expressions";

export class CallTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
    const chain = node.findFirstExpression(abaplint.Expressions.MethodCallChain);
    if (chain) {
      return new MethodCallChainTranspiler().transpile(chain, spaghetti, filename) + ";";
    }

    throw new Error("CallTranspiler, todo");
  }

}