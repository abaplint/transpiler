import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";

export class CorrespondingBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("CorrespondingBodyTranspiler, Expected TypeNameOrInfer");
    }

    if (body.getChildren().length !== 1) {
      throw new Error("CorrespondingBodyTranspiler, Expected single child, todo");
    }

    const source = traversal.traverse(body.getChildren()[0]);
    const type = new TypeNameOrInfer().findType(typ, traversal);
    const target = TranspileTypes.toType(type);

    const ret = new Chunk();
    ret.appendString(`abap.statements.moveCorresponding(${source.getCode()}, ${target})`);
    return ret;
  }

}