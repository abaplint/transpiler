import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class GetRunTimeTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    const ret = new Chunk();
    ret.append("abap.statements.getRunTime(", node, traversal);
    ret.appendChunk(target);
    ret.append(");", node.getLastToken(), traversal);
    return ret;
  }

}