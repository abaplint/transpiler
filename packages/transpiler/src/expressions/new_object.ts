import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";

export class NewObjectTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const typeNameOrInfer = node.findDirectExpression(Expressions.TypeNameOrInfer);
    if (typeNameOrInfer === undefined) {
      throw new Error("NewObjectTranspiler: TypeNameOrInfer not found");
    }

    const type = new TypeNameOrInfer().findType(typeNameOrInfer, traversal);
    console.dir(type);

    ret.appendString("SDFSDFSDF");

    return ret;
  }

}