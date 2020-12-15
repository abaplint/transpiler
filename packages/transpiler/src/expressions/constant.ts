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
      let res = str.getFirstToken().getStr();
      res = res.replace(/\\/g, "\\\\");
      // hmm, how to do this properly?
      res = res.replace(/(.+)''(.+)/g, "$1\\'$2");
      res = res.replace(/(.+)''(.+)/g, "$1\\'$2");
      return res;
    }

    return "todo, Constant";
  }

}