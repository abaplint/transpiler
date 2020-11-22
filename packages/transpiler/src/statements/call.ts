import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class CallTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    if (node.concatTokens().toLowerCase().startsWith("super->constructor(")) {
      // todo, https://github.com/abaplint/transpiler/issues/133
      return "";
    }

    const chain = node.findFirstExpression(abaplint.Expressions.MethodCallChain);
    if (chain) {
      return traversal.traverse(chain) + ";";
    }

    throw new Error("CallTranspiler, todo");
  }

}