import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class FieldLengthTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    const children = node.getChildren();
    const str = children[1].getFirstToken().getStr();
    if (/^\d+$/.test(str)) {
      return str;
    } else {
      return str + ".get()";
    }
  }

}