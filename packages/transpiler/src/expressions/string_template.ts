import {Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class StringTemplateTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
// todo
    let original = node.getFirstToken().getStr();

    original = original.substring(1, original.length - 1);

    return "`" + original + "`";
  }

}