import {Expressions, Nodes, Tokens} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {FieldSymbolTranspiler} from "./field_symbol";

export class FieldLengthTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";
    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        if (c.get() instanceof Expressions.SourceField) {
          let sourceStr = c.getFirstToken().getStr();
          if (sourceStr === "sy") {
            sourceStr = "abap.builtin.sy";
          }
          ret += sourceStr;
        } else if (c.get() instanceof Expressions.SourceFieldSymbol) {
          ret += new FieldSymbolTranspiler().transpile(c, traversal);
        } else if (c.get() instanceof Expressions.ArrowOrDash) {
          ret += ".get().";
        } else if (c.get() instanceof Expressions.ComponentName) {
          ret += c.getFirstToken().getStr();
        }
      } else if(c instanceof Nodes.TokenNode) {
        if (c.get() instanceof Tokens.Identifier) {
          ret += c.getFirstToken().getStr();
        }
      }
    }
    if (/^\d+$/.test(ret)) {
      return ret;
    } else {
      return ret + ".get()";
    }
  }

}