import {AbstractType, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";

export class TypeNameOrInfer implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let type: AbstractType | undefined;

    if (node.concatTokens() === "#") {
      return new Chunk().appendString("inferred,TypeNameOrInfer");
    } else {
      const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
      type = traversal.lookupType(node.getFirstChild() as Nodes.ExpressionNode, scope);
    }

    if (type === undefined) {
      throw new Error("TypeNameOrInfer, type not found: " + node.concatTokens());
    }

    const ret = new Chunk();
    ret.appendString(new TranspileTypes().toType(type));
    return ret;
  }

}