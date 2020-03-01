import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class TypeTableTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {

    const typeName = node.findDirectExpression(Expressions.TypeName);
    if (typeName) {
      return "abap.types.Table";
    }

    return "todo, TypeTableTranspiler";

  }

}