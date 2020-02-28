import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import * as BasicTypes from "../../runtime/types";

export class TypeTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {

    const typeName = node.findDirectExpression(Expressions.TypeName);
    switch (typeName?.getFirstToken().getStr().toUpperCase()) {
      case "I":
        return new BasicTypes.Integer().constructor.name;
      case "C":
        return new BasicTypes.Character().constructor.name;
      case "P":
        return new BasicTypes.Packed().constructor.name;
      case "STRING":
        return new BasicTypes.String().constructor.name;
      default:
        return "todo, TypeTranspiler";
    }

  }

}