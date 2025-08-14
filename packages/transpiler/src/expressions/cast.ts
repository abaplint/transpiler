import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";

export class CastTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const source = node.findDirectExpression(Expressions.Source);
    if (source === undefined) {
      throw new Error("CastTranspiler, Source not found");
    }

    const tni = node.findDirectExpression(Expressions.TypeNameOrInfer);
    if (tni === undefined) {
      throw new Error("CastTranspiler, TypeNameOrInfer not found");
    }
    const type = new TypeNameOrInfer().findType(tni, traversal);
    const target = TranspileTypes.toType(type);

    const lett = node.findDirectExpression(Expressions.Let);
    if (lett !== undefined) {
      throw new Error("CastTranspiler, Let todo");
    }

    ret.appendString("await abap.statements.cast(")
      .appendString(target)
      .appendString(", ")
      .appendChunk(traversal.traverse(source))
      .append(")", node.getLastToken(), traversal);

    return ret;
  }

}