import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class FieldChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    let ret = "";
    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.SourceField
          || c.get() instanceof Expressions.ComponentName) {
        ret = ret + c.getFirstToken().getStr();
      } else if (c instanceof Nodes.TokenNode) {
        const str = c.getFirstToken().getStr();
        if (str === "-") {
          ret = ret + ".";
        }
      }
    }

    return ret;
  }

}