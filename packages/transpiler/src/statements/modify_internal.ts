import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {SourceTranspiler} from "../expressions/index.js";
import {Chunk} from "../chunk.js";

export class ModifyInternalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();

    const extra: string[] = [];

    const index = node.findExpressionAfterToken("INDEX");
    if (index) {
      const s = new SourceTranspiler().transpile(index, traversal).getCode();
      extra.push("index: " + s);
    }

    const from = node.findExpressionAfterToken("FROM");
    if (from) {
      const s = new SourceTranspiler().transpile(from, traversal).getCode();
      extra.push("from: " + s);
    }

    let concat = "";
    if (extra.length > 0) {
      concat = ",{" + extra.join(",") + "}";
    }

    return new Chunk()
      .append("abap.statements.modifyInternal(", node, traversal)
      .appendString(target + concat)
      .append(");", node.getLastToken(), traversal);
  }

}