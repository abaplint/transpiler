import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class NewObjectTranspiler implements IExpressionTranspiler {

  public transpile(_node: Nodes.ExpressionNode, _traversal: Traversal): Chunk {
    const ret = new Chunk();

    ret.appendString("SDFSDFSDF");

    return ret;
  }

}