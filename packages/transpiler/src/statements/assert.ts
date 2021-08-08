import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AssertTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    ret.append("abap.statements.assert(", node, traversal);
    ret.appendChunk(traversal.traverse(node.findDirectExpression(abaplint.Expressions.Cond)));
    ret.append(");", node.getLastToken(), traversal);
    return ret;
  }

}