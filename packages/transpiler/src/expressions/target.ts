import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {FieldLengthTranspiler, FieldOffsetTranspiler, FieldSymbolTranspiler} from ".";
import {Chunk} from "../chunk";

export class TargetTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const offset: string[] = [];
    let ret = new Chunk();

    const children = node.getChildren();
    for (let i = 0; i < children.length; i++) {
      const c = children[i];
      const next: Nodes.ExpressionNode | Nodes.TokenNode | undefined = children[i + 1];

      if (c.get() instanceof Expressions.TargetField) {
        const prefix = traversal.prefixAndName(c.getFirstToken()).replace("~", "$");
        ret.append(Traversal.escapeNamespace(prefix)!, c, traversal);
      } else if (c.get() instanceof Expressions.ClassName) {
        const name = traversal.lookupClassOrInterface(c.getFirstToken().getStr(), c.getFirstToken());
        ret.append(name, c, traversal);
      } else if (c.get() instanceof Expressions.ComponentName) {
        const name = c.getFirstToken().getStr().toLowerCase();
        if (name.match(/^\d/) || name.includes("/")) {
          ret.append(`["` + name + `"]`, c, traversal);
        } else {
          ret.append(`.` + name, c, traversal);
        }
      } else if (c.get() instanceof Expressions.AttributeName) {
        const intf = Traversal.escapeNamespace(traversal.isInterfaceAttribute(c.getFirstToken()));
        let name = Traversal.escapeNamespace(c.getFirstToken().getStr())!.replace("~", "$").toLowerCase();
        if (intf && name.startsWith(intf) === false) {
          name = intf + "$" + name;
        }
        ret.append(name, c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldOffset) {
        offset.push("offset: " + new FieldOffsetTranspiler().transpile(c, traversal).getCode());
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldLength) {
        const len = new FieldLengthTranspiler().transpile(c, traversal).getCode();
        if (len !== "*") {
          offset.push("length: " + len);
        }
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.TargetFieldSymbol) {
        ret.appendChunk(new FieldSymbolTranspiler().transpile(c, traversal));
      } else if (c.getFirstToken().getStr() === "-") {
        ret.append(".get()", c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Dereference) {
        ret.append(".dereference()", c, traversal);
      } else if (c.getFirstToken().getStr() === "=>") {
        ret.append(".", c, traversal);
      } else if (c.getFirstToken().getStr() === "->") {
        if (next.concatTokens() === "*") {
          ret.append(".dereference()", c, traversal);
        } else {
          ret.append(".get().", c, traversal);
        }
      }
    }

    if (offset.length > 0) {
      const pre = "new abap.OffsetLength(";
      const post = ", {" + offset.join(", ") + "})";
      ret = new Chunk().appendString(pre).appendChunk(ret).appendString(post);
    }

    return ret;
  }

}