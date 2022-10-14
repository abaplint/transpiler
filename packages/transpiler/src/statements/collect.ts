import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CollectTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    const fstarget = node.findDirectExpression(abaplint.Expressions.FSTarget);
    if (fstarget) {
      return new Chunk(`throw new Error("Collect, transpiler todo");`);
    }

    return new Chunk()
      .appendString("abap.statements.collect(")
      .appendChunk(source)
      .appendString(",")
      .appendChunk(target)
      .appendString(");");
  }

}