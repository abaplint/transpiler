import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class CondenseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    const noGaps = node.concatTokens().toUpperCase().includes(" NO-GAPS");

    const ret = new Chunk();
    ret.append("abap.statements.condense(", node, traversal);
    ret.appendChunk(target);
    ret.append(", {nogaps: " + noGaps + "});", node.getLastToken(), traversal);
    return ret;
  }

}