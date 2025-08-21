import {AbstractType, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";

export class TypeNameOrInfer implements IExpressionTranspiler {

  public findType(node: Nodes.ExpressionNode, traversal: Traversal): AbstractType {
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const type = traversal.lookupInferred(node, scope);

    if (type === undefined) {
      throw new Error("TypeNameOrInfer, type not found: " + node.concatTokens() + ", " + traversal.getCurrentObject().getName() + " line " + node.getFirstToken().getStart().getRow());
    }

    return type;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const type = this.findType(node, traversal);

    const ret = new Chunk();
    ret.appendString(TranspileTypes.toType(type));
    return ret;
  }

}