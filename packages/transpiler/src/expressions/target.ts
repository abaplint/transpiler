import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {FieldSymbolTranspiler} from "./field_symbol";

export class TargetTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.TargetField) {
        if (traversal.isClassAttribute(c.getFirstToken())) {
          ret = "this.";
        }
        ret = ret + c.getFirstToken().getStr();
      } else if (c.get() instanceof Expressions.ComponentName) {
        ret = ret + c.getFirstToken().getStr();
      } else if (c instanceof Nodes.ExpressionNode
          && c.get() instanceof Expressions.TargetFieldSymbol) {
        ret = ret + new FieldSymbolTranspiler().transpile(c, traversal);
      } else if (c.getFirstToken().getStr() === "-") {
        ret = ret + ".get().";
      }
    }

    return ret;
  }

}