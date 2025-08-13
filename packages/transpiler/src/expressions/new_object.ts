import {Nodes, Expressions} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";

export class NewObjectTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const typeNameOrInfer = node.findDirectExpression(Expressions.TypeNameOrInfer);
    if (typeNameOrInfer === undefined) {
      throw new Error("NewObjectTranspiler: TypeNameOrInfer not found");
    }

    let para = "";
    const parameters = node.findFirstExpression(abaplint.Expressions.ParameterListS);
    if (parameters) {
      para = traversal.traverse(parameters).getCode();
    }

    const type = new TypeNameOrInfer().findType(typeNameOrInfer, traversal);
    if (type instanceof abaplint.BasicTypes.ObjectReferenceType) {
      const clas = traversal.lookupClassOrInterface(type.getIdentifierName(), node.getFirstToken());
      ret.appendString(TranspileTypes.toType(type) + ".set(await (new " + clas + "()).constructor_(" + para + "))");
    } else {
      throw new Error("NewObjectTranspiler: only ObjectReferenceType currently handled, todo");
    }

    return ret;
  }

}