import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ClearTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    // Reference implementation for the source-map convention:
    // - keep the target's own expression-level mappings by appending its Chunk
    //   (do NOT flatten with target.getCode(), that discards the mappings)
    // - map the trailing generated syntax to the statement's last token, not
    //   getEnd() which points one column past the statement
    const ret = new Chunk();
    ret.appendChunk(target);
    ret.append(".clear();", node.getLastToken(), traversal);
    return ret;
  }

}