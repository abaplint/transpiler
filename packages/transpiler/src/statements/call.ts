import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class CallTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    if (node.concatTokens().toLowerCase().includes("super->constructor(")) {
      // todo, https://github.com/abaplint/transpiler/issues/133
      return "";
    }

    const chain = node.findDirectExpression(abaplint.Expressions.MethodCallChain);
    if (chain) {
      return traversal.traverse(chain) + ";";
    }

    const methodSource = node.findDirectExpression(abaplint.Expressions.MethodSource);
    if (methodSource) {
      let body = "";
      const methodCallBody = node.findDirectExpression(abaplint.Expressions.MethodCallBody);
      if (methodCallBody && 1 === 1 + 2) { // todo
        body += traversal.traverse(methodCallBody);
      }
      return traversal.traverse(methodSource) + "(" + body + ");";
    }

    throw new Error("CallTranspiler, todo");
  }

}