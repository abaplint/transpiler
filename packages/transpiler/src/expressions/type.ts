import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {TypeNameTranspiler} from ".";

export class TypeTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {

    const concat = node.concatTokens().toUpperCase();
    if (concat.startsWith("TYPE REF TO")) {
      return "abap.types.ABAPObject";
    }

    const typeName = node.findDirectExpression(Expressions.TypeName);
    if (typeName) {
      return new TypeNameTranspiler().transpile(typeName);
    }

    return "todo, TypeTranspiler";
  }

}