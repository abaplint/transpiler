import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class IfTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const cond = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Cond));
    const ret = new Chunk();
    ret.append("if (", node, traversal);
    ret.appendChunk(cond);
    ret.append(") {", node.getLastToken(), traversal);
    return ret;
  }

}