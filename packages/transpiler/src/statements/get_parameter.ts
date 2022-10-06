import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class GetParameterTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    return new Chunk(`abap.statements.getParameter(`)
      .appendChunk(source)
      .appendString(",")
      .appendChunk(target)
      .appendString(");");
  }

}