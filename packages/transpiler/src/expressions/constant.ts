import {Expressions, Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ConstantTranspiler implements IExpressionTranspiler {
  private readonly addGet: boolean;

  public constructor(addGet = false) {
    this.addGet = addGet;
  }

  public transpile(node: Nodes.ExpressionNode, _traversal: Traversal): Chunk {
    const int = node.findFirstExpression(Expressions.Integer);
    if (int) {
      const val = parseInt(int.concatTokens(), 10);
      let ret = "constant_" + (val < 0 ? "minus_" : "") + Math.abs(val);
      if (this.addGet === true) {
        ret += ".get()";
      }
      return new Chunk(ret);
    }

    const str = node.findFirstExpression(Expressions.ConstantString);
    if (str) {
      const res = str.getFirstToken().getStr();
      if (res.startsWith("'") && this.addGet === false) {
        return new Chunk("new abap.types.Character({length: " + (res.length - 2) + "}).set(" + this.escape(res) + ")");
      } else {
        return new Chunk(this.escape(res));
      }
    }

    return new Chunk("todo, Constant");
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