import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {FieldLengthTranspiler, FieldOffsetTranspiler} from ".";
import {Traversal} from "../traversal";
import {FieldSymbolTranspiler} from "./field_symbol";
import {Chunk} from "../chunk";

export class FieldChainTranspiler implements IExpressionTranspiler {
  private readonly addGet: boolean;
  private addGetOffset: boolean;

  public constructor(addGet = false) {
    this.addGet = addGet;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, prefix = true): Chunk {
    let ret = "";
    const extra: string[] = [];

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.SourceField
          || c.get() instanceof Expressions.Field) {
        ret = ret + traversal.findPrefix(c.getFirstToken()).replace("~", "$");
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.SourceFieldSymbol) {
        ret = ret + new FieldSymbolTranspiler().transpile(c, traversal).getCode();
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ClassName) {
        ret += traversal.lookupClassOrInterface(c.getFirstToken().getStr(), c.getFirstToken()) + ".";
      } else if (c.get() instanceof Expressions.AttributeName) {
        const interfaceName = traversal.isInterfaceAttribute(c.getFirstToken());
        let name = c.getFirstToken().getStr().replace("~", "$");
        if (prefix && interfaceName && name.startsWith(interfaceName) === false) {
          name = interfaceName + "$" + name;
        }
        ret += name;
      } else if (c.get() instanceof Expressions.ComponentName) {
        ret = ret + c.getFirstToken().getStr().toLowerCase();
      } else if (c instanceof Nodes.TokenNode) {
        const str = c.getFirstToken().getStr();
        if (str === "-" || str === "->") {
          ret += ".get().";
        }
      } else if (c instanceof Nodes.ExpressionNode
          && c.get() instanceof Expressions.FieldOffset) {
        extra.push("offset: " + new FieldOffsetTranspiler().transpile(c, traversal).getCode());
        this.addGetOffset = true;
      } else if (c instanceof Nodes.ExpressionNode
          && c.get() instanceof Expressions.FieldLength) {
        extra.push("length: " + new FieldLengthTranspiler().transpile(c, traversal).getCode());
        this.addGetOffset = true;
      }
    }

    if (this.addGetOffset) {
      let foo = extra.join(", ");
      if (foo !== "") {
        foo = "{" + foo + "}";
      }
      ret = ret + ".getOffset(" + foo + ")";  // todo, this will break
    }
    if (this.addGet) {
      ret = ret + ".get()";  // todo, this will break?
    }

    return new Chunk(ret);
  }

}