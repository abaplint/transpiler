import {Expressions, Nodes, Tokens} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {FieldSymbolTranspiler} from "./field_symbol";
import {Chunk} from "../chunk";

export class FieldOffsetTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = "";
    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        if (c.get() instanceof Expressions.SourceField) {
          let sourceStr = c.getFirstToken().getStr().toLowerCase();
          if (sourceStr === "sy") {
            sourceStr = "abap.builtin.sy";
          }
          ret += sourceStr;
        } else if (c.get() instanceof Expressions.SourceFieldSymbol) {
          ret += new FieldSymbolTranspiler().transpile(c, traversal).getCode();
        } else if (c.get() instanceof Expressions.ArrowOrDash) {
          ret += ".get()";
        } else if (c.get() instanceof Expressions.ComponentName) {
          const name = c.getFirstToken().getStr().toLowerCase();
          if (name.match(/^\d/)) {
            ret += `["` + name + `"]`;
          } else {
            ret += `.` + name;
          }
        }
      } else if(c instanceof Nodes.TokenNode) {
        if (c.get() instanceof Tokens.Identifier) {
          ret += c.getFirstToken().getStr().toLowerCase();
        }
      }
    }
    return new Chunk(ret);
  }

}