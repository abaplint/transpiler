import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class FieldChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    const name = node.findFirstExpression(Expressions.SourceField);
    if (name) {
      return name.getFirstToken().getStr();
    }

    return "todo, FieldChain";
  }

}