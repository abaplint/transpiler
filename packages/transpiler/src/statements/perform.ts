import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class PerformTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, _traversal: Traversal): Chunk {
    const formName = node.findDirectExpression(abaplint.Expressions.FormName);
    if (formName === undefined) {
      throw new Error("PerformTranspiler, FormName not found");
    }

    // todo, parameters

    if (node.concatTokens().includes(" IN PROGRAM ")) {
      return new Chunk(`throw new Error("PerformTranspiler IN PROGRAM, transpiler todo");`);
    }

    return new Chunk("await " + formName.concatTokens() + "();");
  }

}