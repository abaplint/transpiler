import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class SubtractTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));

    const ret = new Chunk();
    ret.appendChunk(target);
    ret.append(".set(abap.operators.minus(", node, traversal);
    ret.appendChunk(target);
    ret.append(",", node, traversal);
    ret.appendChunk(source);
    ret.append("));", node.getLastToken(), traversal);
    return ret;
  }

}