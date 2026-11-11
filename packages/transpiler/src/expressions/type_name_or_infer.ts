import {AbstractType, BasicTypes, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {UnknownTypesEnum} from "../types";

export class TypeNameOrInfer implements IExpressionTranspiler {

  public findType(node: Nodes.ExpressionNode, traversal: Traversal): AbstractType {
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const type = traversal.lookupInferred(node, scope);

    if (type === undefined) {
        if (node.concatTokens() === "#") {
      const sqlType = traversal.getSQLInferredType();
      if (sqlType !== undefined) {
        return sqlType;
      }
    }
      const text = "TypeNameOrInfer, type not found: " + node.concatTokens() + ", " + traversal.getCurrentObject().getName() + ", " + traversal.getFilename() + " line " + node.getFirstToken().getStart().getRow();
      if (traversal.options?.unknownTypes === UnknownTypesEnum.runtimeError) {
        // eg. # for a parameter of a class that is not there: the code fails when it runs, as with other unknown types
        return new BasicTypes.UnknownType(text);
      }
      throw new Error(text);
    }

    return type;
  }

  public findTypeOrUndefined(node: Nodes.ExpressionNode, traversal: Traversal): AbstractType | undefined {
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const type = traversal.lookupInferred(node, scope);
    return type;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const type = this.findType(node, traversal);

    const ret = new Chunk();
    ret.appendString(traversal.toType(type));
    return ret;
  }

}
