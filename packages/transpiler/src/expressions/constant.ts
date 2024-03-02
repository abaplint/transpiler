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
      const parsed = Number.parseInt(concat, 10);
      let code = "";
      if (concat.length > 18) {
        // its potentially larger than Number.MAX_SAFE_INTEGER
        // https://stackoverflow.com/questions/1379934/large-numbers-erroneously-rounded-in-javascript
        code = `new abap.types.Integer8().set("${concat}")`;
      } else if (parsed > 2147483647 || parsed < -2147483648) {
        code = `new abap.types.Integer8().set(${concat})`;
      } else if (parsed >= -10 && parsed <= 200) {
        code = `abap.IntegerFactory.get(${concat})`;
      } else {
        code = `new abap.types.Integer().set(${concat})`;
      }
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
        const code = "new abap.types.String().set(" + ConstantTranspiler.escape(res) + ")";
        return new Chunk().append(code, node, traversal);
      } else {
        if (res.startsWith("'")) {
          res = "'" + res.substring(1, res.length - 1).trimEnd() + "'";
        }
        const code = ConstantTranspiler.escape(res);
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
          const code = "new abap.types.String().set(" + ConstantTranspiler.escape(res) + ")";
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
    const code = "new abap.types.Character(" + length + ").set(" + ConstantTranspiler.escape(res) + ")";
    return code;
  }

  public static escape(str: string): string {
    str = str.replace(/\\/g, "\\\\");

    if (str.startsWith("'")) {
      const reg = new RegExp(/(.+)''(.+)/g);
      while (reg.test(str)) {
        str = str.replace(reg, "$1\\'$2");
      }
    } else if (str.startsWith("`")) {
      const reg = new RegExp(/(.+)``(.+)/g);
      while (reg.test(str)) {
        str = str.replace(reg, "$1\\`$2");
      }
      str = str.replace(/\$\{/g, "\\${");
    } else if (str.includes("\n")
        || str.includes("\r")
        || str.includes("\r")
        || str.includes("\t")
        || str.includes("\f")
        || str.includes("\v")
        || str.includes("\b")) {
      str = JSON.stringify(str);
    }

    return str;
  }

}