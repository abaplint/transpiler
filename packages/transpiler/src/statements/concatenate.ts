import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class ConcatenateTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const concat = node.concatTokens().toUpperCase();

    const slist: Chunk[] = [];
    for (const s of node.findDirectExpressions(abaplint.Expressions.Source)) {
      slist.push(traversal.traverse(s));
    }

    let extra = "";
    if (node.findExpressionAfterToken("BY")) {
      extra = `, separatedBy: ${slist.pop()?.getCode()}`;
    }

    if (concat.startsWith("CONCATENATE LINES OF ")) {
      extra += ", lines: true";
    }
    if (concat.includes(" RESPECTING BLANKS")) {
      extra += ", respectingBlanks: true";
    }

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    return new Chunk()
      .append("abap.statements.concatenate({source: [", node, traversal)
      .join(slist)
      .appendString("], target: ")
      .appendChunk(target)
      .append(extra + "});", node.getLastToken(), traversal);
  }

}