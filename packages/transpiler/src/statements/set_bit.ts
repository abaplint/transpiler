import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class SetBitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const sources = node.findDirectExpressions(abaplint.Expressions.Source);
    const source = traversal.traverse(sources[0]).getCode();
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();
    const value = sources[1] ? traversal.traverse(sources[1]).getCode() : undefined;

    if (value) {
      return new Chunk("abap.statements.setBit(" + source + ", " + target + ", " + value + ");");
    } else {
      return new Chunk("abap.statements.setBit(" + source + ", " + target + ");");
    }
  }

}