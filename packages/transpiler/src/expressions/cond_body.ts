import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CondBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, _body: Nodes.ExpressionNode, _traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("CondBodyTranspiler, Expected TypeNameOrInfer");
    }

    const ret = new Chunk();
    // todo
    return ret;
  }

}