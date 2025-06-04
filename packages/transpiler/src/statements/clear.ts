import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ClearTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    const ret = new Chunk();

    /*
    ret.appendChunk(target);
    ret.append(".clear();", node.getLastToken().getEnd(), traversal);
    */

    ret.append(target.getCode() + ".clear();", node.getLastToken().getEnd(), traversal);

//    ret.append(target.getCode() + ".clear();", node, traversal);

/*
    ret.append(target.getCode(), node.getFirstToken().getStart(), traversal);
    ret.append(".clear();", node.getLastToken().getEnd(), traversal);
    */
    return ret;
  }

}