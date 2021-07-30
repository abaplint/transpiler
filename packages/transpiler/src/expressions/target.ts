import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {FieldLengthTranspiler, FieldOffsetTranspiler, FieldSymbolTranspiler} from ".";

export class TargetTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    const offset: string[] = [];
    let ret = "";

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.TargetField) {
        ret = ret + traversal.findPrefix(c.getFirstToken());
        ret = ret.replace("~", "$");
      } else if (c.get() instanceof Expressions.ClassName) {
        ret += traversal.lookupClassOrInterface(c.getFirstToken().getStr(), c.getFirstToken());
      } else if (c.get() instanceof Expressions.ComponentName) {
        ret = ret + c.getFirstToken().getStr().toLowerCase();
      } else if (c.get() instanceof Expressions.AttributeName) {
        const intf = traversal.isInterfaceAttribute(c.getFirstToken());
        let name = c.getFirstToken().getStr().replace("~", "$").toLowerCase();
        if (intf && name.startsWith(intf) === false) {
          name = intf + "$" + name;
        }
        ret += name;
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldOffset) {
        offset.push("offset: " + new FieldOffsetTranspiler().transpile(c, traversal));
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldLength) {
        offset.push("length: " + new FieldLengthTranspiler().transpile(c, traversal));
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.TargetFieldSymbol) {
        ret = ret + new FieldSymbolTranspiler().transpile(c, traversal);
      } else if (c.getFirstToken().getStr() === "-") {
        ret = ret + ".get().";
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Dereference) {
        ret = ret + ".getPointer()";
        break;
      } else if (c.getFirstToken().getStr() === "=>") {
        ret = ret + ".";
      } else if (c.getFirstToken().getStr() === "->") {
        if (node.concatTokens().endsWith("->*")) {
          ret = ret + ".getPointer()";
          break;
        } else {
          ret = ret + ".get().";
        }
      }
    }

    let pre = "";
    let post = "";
    if (offset.length > 0) {
      pre = "new abap.OffsetLength(";
      post = ", {" + offset.join(", ") + "})";
    }

    return pre + ret + post;
  }

}