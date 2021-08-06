import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CallTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    if (node.concatTokens().toLowerCase().includes("super->constructor")) {
      // todo, https://github.com/abaplint/transpiler/issues/133
      return new Chunk("");
    }


    const chain = node.findDirectExpression(abaplint.Expressions.MethodCallChain);
    if (chain) {
      return new Chunk(traversal.traverse(chain).getCode() + ";");
    }

    const methodSource = node.findDirectExpression(abaplint.Expressions.MethodSource);
    if (methodSource) {
      let body = "";
      const methodCallBody = node.findDirectExpression(abaplint.Expressions.MethodCallBody);
      if (methodCallBody && 1 === 1 + 2) { // todo
        body += traversal.traverse(methodCallBody).getCode();
      }
      return new Chunk(traversal.traverse(methodSource).getCode() + "(" + body + ");");
    }

    throw new Error("CallTranspiler, todo");
  }

}