import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class SplitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const sources = node.findDirectExpressions(abaplint.Expressions.Source);

    const source = traversal.traverse(sources[0]).getCode();
    const at = traversal.traverse(sources[1]).getCode();

    let to = "";
    const table = node.findExpressionAfterToken("TABLE");
    if (table) {
      const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();
      to = ", table: " + target;
    } else {
      to = ", targets: [" + node.findDirectExpressions(abaplint.Expressions.Target).map(e => traversal.traverse(e).getCode()).join(",") + "]";
    }

    return new Chunk().append("abap.statements.split({source: " + source + ", at: " + at + to + "});", node, traversal);
  }

}