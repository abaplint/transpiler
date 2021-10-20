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

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, prefix = true, filename?: string): Chunk {
    const ret = new Chunk();
    const extra: string[] = [];

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.SourceField
          || c.get() instanceof Expressions.Field) {
        const name = traversal.prefixAndName(c.getFirstToken(), filename).replace("~", "$");
        ret.append(name, c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.SourceFieldSymbol) {
        ret.appendChunk(new FieldSymbolTranspiler().transpile(c, traversal));
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ClassName) {
        const name = traversal.lookupClassOrInterface(c.getFirstToken().getStr(), c.getFirstToken());
        ret.append(name + ".", c, traversal);
      } else if (c.get() instanceof Expressions.AttributeName) {
        const interfaceName = traversal.isInterfaceAttribute(c.getFirstToken());
        let name = c.getFirstToken().getStr().replace("~", "$");
        if (prefix && interfaceName && name.startsWith(interfaceName) === false) {
          name = interfaceName + "$" + name;
        }
        ret.append(name, c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Dereference) {
        ret.append(".getPointer()", c, traversal);
      } else if (c.get() instanceof Expressions.ComponentName) {
        ret.append(c.getFirstToken().getStr().toLowerCase(), c, traversal);
      } else if (c instanceof Nodes.TokenNode) {
        const str = c.getFirstToken().getStr();
        if (str === "-" || str === "->") {
          ret.append(".get().", c, traversal);
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
      ret.appendString(".getOffset(" + foo + ")");  // todo, this will break
    }
    if (this.addGet) {
      ret.appendString(".get()");  // todo, this will break?
    }

    return ret;
  }

}