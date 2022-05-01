import {Expressions, Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ConstantTranspiler implements IExpressionTranspiler {
  private readonly addGet: boolean;

  public constructor(addGet = false) {
    this.addGet = addGet;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const int = node.findFirstExpression(Expressions.Integer);
    if (int) {
      const concat = int.concatTokens().trim();
      const post = concat.startsWith("-") ? "minus_" : "";
      let ret = "constant_" + post + int.getLastToken().getStr();
      if (this.addGet === true) {
        ret += ".get()";
      }
      return new Chunk().append(ret, node, traversal);
    }

    let str = node.findFirstExpression(Expressions.ConstantString);
    if (str === undefined) {
      str = node.findFirstExpression(Expressions.TextElementString);
    }
    if (str) {
      let res = str.getFirstToken().getStr();
      if (res.startsWith("'") && this.addGet === false) {
        const code = "new abap.types.Character({length: " + (res.length - 2) + "}).set(" + this.escape(res) + ")";
        return new Chunk().append(code, node, traversal);
      } else if (res.startsWith("`") && this.addGet === false) {
        const code = "new abap.types.String().set(" + this.escape(res) + ")";
        return new Chunk().append(code, node, traversal);
      } else {
        if (res.startsWith("'")) {
          res = "'" + res.substring(1, res.length - 1).trimEnd() + "'";
        }
        const code = this.escape(res);
        return new Chunk().append(code, node, traversal);
      }
    }

    return new Chunk(`todo, Constant`);
  }

  public escape(str: string): string {
    str = str.replace(/\\/g, "\\\\");

    if (str.startsWith("'")) {
      const reg = new RegExp(/(.+)''(.+)/g);
      while (reg.test(str)) {
        str = str.replace(reg, "$1\\'$2");
      }
    }

    if (str.startsWith("`")) {
      const reg = new RegExp(/(.+)``(.+)/g);
      while (reg.test(str)) {
        str = str.replace(reg, "$1\\`$2");
      }
    }

    return str;
  }

}