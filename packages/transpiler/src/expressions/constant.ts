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
      let code = `new abap.types.Integer().set(${concat})`;
      if (this.addGet === true) {
        code += ".get()";
      }
      return new Chunk().append(code, node, traversal);
    }

    let str = node.findDirectExpression(Expressions.ConstantString);
    if (str === undefined) {
      str = node.findDirectExpression(Expressions.TextElementString);
    }
    if (str) {
      let res = str.getFirstToken().getStr();
      if (res.startsWith("'") && this.addGet === false) {
        const code = this.character(res);
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

    const concat = node.findDirectExpression(Expressions.ConcatenatedConstant);
    if (concat) {
      const chunk = new Chunk().appendString("abap.operators.concat([");
      let first = true;
      for (const child of concat.getChildren()) {
        const res = child.getFirstToken().getStr();
        if (first === true) {
          first = false;
        } else if (res !== "&"){
          chunk.appendString(",");
        }
        if (res.startsWith("'") && this.addGet === false) {
          const code = this.character(res);
          chunk.append(code, node, traversal);
        } else if (res.startsWith("`") && this.addGet === false) {
          const code = "new abap.types.String().set(" + this.escape(res) + ")";
          chunk.append(code, node, traversal);
        }
      }
      chunk.appendString("])");
      return chunk;
    }

    return new Chunk(`todo, Constant`);
  }

  private character(res: string): string {
    const foo = res.replace(/''/g, "'");
    let length = foo.length - 2;
    if (length <= 0) {
      // note: Characters cannot have length = zero, 1 is minimum
      length = 1;
    }
    const code = "new abap.types.Character(" + length + ").set(" + this.escape(res) + ")";
    return code;
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
      str = str.replace(/\$\{/g, "\\${");
    }

    return str;
  }

}