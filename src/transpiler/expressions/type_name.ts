import {Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import * as BasicTypes from "../../runtime/types";

export class TypeNameTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {

    switch (node?.getFirstToken().getStr().toUpperCase()) {
      case "I":
        return "abap.types." + new BasicTypes.Integer().constructor.name;
      case "C":
        return "abap.types." + new BasicTypes.Character().constructor.name;
      case "P":
        return "abap.types." + new BasicTypes.Packed().constructor.name;
      case "STRING":
        return "abap.types." + new BasicTypes.String().constructor.name;
      default:
        return "todo, TypeNameTranspiler";
    }

  }

}