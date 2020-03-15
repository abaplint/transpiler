import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {FieldLengthTranspiler, FieldOffsetTranspiler} from ".";
import {Traversal} from "../traversal";

export class FieldChainTranspiler implements IExpressionTranspiler {
  private addGet: boolean;

  public constructor(addGet = false) {
    this.addGet = addGet;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";
    const extra: string[] = [];

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.SourceField) {
        let name = c.getFirstToken().getStr();
        if (traversal.isClassAttribute(c.getFirstToken())) {
          name = "this." + name;
        }

        ret = ret + name;
      } else if (c.get() instanceof Expressions.ComponentName) {
        ret = ret + c.getFirstToken().getStr();
      } else if (c instanceof Nodes.TokenNode) {
        const str = c.getFirstToken().getStr();
        if (str === "-") {
          ret = ret + ".get().";
        }
      } else if (c instanceof Nodes.ExpressionNode
          && c.get() instanceof Expressions.FieldOffset) {
        extra.push("offset: " + new FieldOffsetTranspiler().transpile(c));
        this.addGet = true;
      } else if (c instanceof Nodes.ExpressionNode
          && c.get() instanceof Expressions.FieldLength) {
        extra.push("length: " + new FieldLengthTranspiler().transpile(c));
        this.addGet = true;
      }
    }

    if (this.addGet) {
      let foo = extra.join(", ");
      if (foo !== "") {
        foo = "{" + foo + "}";
      }
      ret = ret + ".get(" + foo + ")";  // todo, this will break
    }

    return ret;
  }

}