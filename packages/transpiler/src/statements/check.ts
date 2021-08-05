import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CheckTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const cond = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Cond)).getCode();
    return new Chunk("if (!(" + cond + ")) {\ncontinue;\n}");
  }

}