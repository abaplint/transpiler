import {Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class TableExpressionTranspiler {

  public transpile(_node: Nodes.ExpressionNode, _traversal: Traversal, source: Chunk): Chunk {
    if (source === undefined) {
      throw new Error("TableExpressionTranspiler: Source chunk is undefined");
    }

    const ret = new Chunk();

    ret.appendString("todo");

    return ret;
  }

}