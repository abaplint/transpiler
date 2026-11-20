import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SetParameterTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const sources = node.findDirectExpressions(abaplint.Expressions.Source);
    if (sources.length !== 2) {
      throw new Error("SetParameterTranspiler, unexpected node");
    }
    return new Chunk(`abap.statements.setParameter(`)
      .appendChunk(traversal.traverse(sources[0]))
      .appendString(",")
      .appendChunk(traversal.traverse(sources[1]))
      .appendString(");");
  }

}
