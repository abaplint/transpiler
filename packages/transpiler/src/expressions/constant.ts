import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ConstantTranspiler implements IExpressionTranspiler {
  private readonly addGet: boolean;

  public constructor(addGet = false) {
    this.addGet = addGet;
  }

  public transpile(node: Nodes.ExpressionNode, _traversal: Traversal): string {
    const int = node.findFirstExpression(Expressions.Integer);
    if (int) {
      const val = parseInt(int.concatTokens(), 10);
      let ret = "constant_" + (val < 0 ? "minus_" : "") + Math.abs(val);
      if (this.addGet === true) {
        ret += ".get()";
      }
      return ret;
    }

    const str = node.findFirstExpression(Expressions.ConstantString);
    if (str) {
      const res = str.getFirstToken().getStr();
      return this.escape(res);
    }

    return "todo, Constant";
  }

  public escape(str: string): string {
    str = str.replace(/\\/g, "\\\\");

    const reg = new RegExp(/(.+)''(.+)/g);
    while (reg.test(str)) {
      str = str.replace(reg, "$1\\'$2");
    }
    return str;
  }

}