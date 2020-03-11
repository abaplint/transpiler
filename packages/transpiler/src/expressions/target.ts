import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class TargetTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.TargetField
          || c.get() instanceof Expressions.FieldAll) {
        ret = ret + c.getFirstToken().getStr();
      } else if (c.get() instanceof Expressions.ArrowOrDash) {
        ret = ret + ".get().";
      }
    }

    return ret;
  }

}