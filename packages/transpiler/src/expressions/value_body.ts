import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";

export class ValueBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("ValueBodyTranspiler, Expected TypeNameOrInfer");
    }

    const ret = new Chunk().appendString(new TypeNameOrInfer().transpile(typ, traversal).getCode());
    /*
    ret.appendString(".set(");
    // todo: handle LET
    ret.appendString(traversal.traverse(body).getCode());
    ret.appendString(")");
    */
    for (const child of body.getChildren()) {
      if (child.get() instanceof Expressions.FieldAssignment) {
        ret.appendString(".set()");
      } else {
        throw new Error("ValueBodyTranspiler, unknown " + child.get().constructor.name);
      }
    }

    return ret;
  }

}