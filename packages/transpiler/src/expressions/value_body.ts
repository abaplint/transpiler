import {Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";

export class ValueBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, _body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    const ret = new Chunk().appendString(new TypeNameOrInfer().transpile(typ, traversal).getCode());
    /*
    ret.appendString(".set(");
    // todo: handle LET
    ret.appendString(traversal.traverse(body).getCode());
    ret.appendString(")");
    */

    return ret;
  }

}