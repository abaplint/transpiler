import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CondenseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    const noGaps = node.concatTokens().includes(" NO-GAPS");

    const ret = new Chunk();
    ret.append("abap.statements.condense(", node, traversal);
    ret.appendChunk(target);
    ret.append(", {nogaps: " + noGaps + "});", node.getLastToken(), traversal);
    return ret;
  }

}