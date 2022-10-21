import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {FieldLengthTranspiler, FieldOffsetTranspiler, FieldSymbolTranspiler} from ".";
import {Chunk} from "../chunk";

export class TargetTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const offset: string[] = [];
    const ret = new Chunk();

    const children = node.getChildren();
    for (let i = 0; i < children.length; i++) {
      const c = children[i];
      const next: Nodes.ExpressionNode | Nodes.TokenNode | undefined = children[i + 1];

      if (c.get() instanceof Expressions.TargetField) {
        const prefix = traversal.prefixAndName(c.getFirstToken()).replace("~", "$");
        ret.append(prefix, c, traversal);
      } else if (c.get() instanceof Expressions.ClassName) {
        const name = traversal.lookupClassOrInterface(c.getFirstToken().getStr(), c.getFirstToken());
        ret.append(name, c, traversal);
      } else if (c.get() instanceof Expressions.ComponentName) {
        const name = c.getFirstToken().getStr().toLowerCase();
        if (name.startsWith("0")) {
          ret.append(`["` + name + `"]`, c, traversal);
        } else {
          ret.append(`.` + name, c, traversal);
        }
      } else if (c.get() instanceof Expressions.AttributeName) {
        const intf = traversal.isInterfaceAttribute(c.getFirstToken());
        let name = Traversal.escapeClassName(c.getFirstToken().getStr())!.replace("~", "$").toLowerCase();
        if (intf && name.startsWith(intf) === false) {
          name = Traversal.escapeClassName(intf) + "$" + name;
        }
        ret.append(name, c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldOffset) {
        offset.push("offset: " + new FieldOffsetTranspiler().transpile(c, traversal).getCode());
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldLength) {
        offset.push("length: " + new FieldLengthTranspiler().transpile(c, traversal).getCode());
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.TargetFieldSymbol) {
        ret.appendChunk(new FieldSymbolTranspiler().transpile(c, traversal));
      } else if (c.getFirstToken().getStr() === "-") {
        ret.append(".get()", c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Dereference) {
        ret.append(".getPointer()", c, traversal);
      } else if (c.getFirstToken().getStr() === "=>") {
        ret.append(".", c, traversal);
      } else if (c.getFirstToken().getStr() === "->") {
        if (next.concatTokens() === "*") {
          ret.append(".getPointer()", c, traversal);
        } else {
          ret.append(".get().", c, traversal);
        }
      }
    }

    let pre = "";
    let post = "";
    if (offset.length > 0) {
      pre = "new abap.OffsetLength(";
      post = ", {" + offset.join(", ") + "})";
    }

    return new Chunk()
      .appendString(pre)
      .appendChunk(ret)
      .appendString(post);
  }

}