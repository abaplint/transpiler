import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";

export class FilterBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("FilterBodyTranspiler, Expected TypeNameOrInfer");
    } else if (body.findDirectExpression(Expressions.Let)) {
      throw new Error("FilterBodyTranspiler, Let not supported, todo");
    }

    const source = traversal.traverse(body.findDirectExpression(Expressions.Source));
    const type = new TypeNameOrInfer().findType(typ, traversal);
    const target = TranspileTypes.toType(type);

    const ret = new Chunk();
    return ret;
  }

}