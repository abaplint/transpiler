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

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, prefix = true, filename?: string, wrongScope = false): Chunk {
    const ret = new Chunk();
    const extra: string[] = [];
    let interfaceNameAdded = false;

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.SourceField
          || c.get() instanceof Expressions.Field) {
        const name = traversal.prefixAndName(c.getFirstToken(), filename).replace("~", "$");
        ret.append(Traversal.prefixVariable(Traversal.escapeNamespace(name)!), c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.SourceFieldSymbol) {
        ret.appendChunk(new FieldSymbolTranspiler().transpile(c, traversal));
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ClassName) {
        const name = traversal.lookupClassOrInterface(c.getFirstToken().getStr(), c.getFirstToken());
        ret.append(name + ".", c, traversal);
        if (wrongScope === true && traversal.reg.getObject("INTF", c.getFirstToken().getStr())) {
          ret.append(Traversal.escapeNamespace(c.getFirstToken().getStr().toLocaleLowerCase())! + "$", c, traversal);
          interfaceNameAdded = true;
        }
      } else if (c.get() instanceof Expressions.AttributeName) {
        const interfaceName = traversal.isInterfaceAttribute(c.getFirstToken());
        let name = c.getFirstToken().getStr()!.toLowerCase();
        if (prefix && interfaceName && name.startsWith(interfaceName) === false && interfaceNameAdded === false) {
          name = Traversal.escapeNamespace(name)!.replace("~", "$");
          name = Traversal.escapeNamespace(interfaceName) + "$" + name;
        } else {
          name = Traversal.escapeNamespace(name)!.replace("~", "$");
        }
        ret.append(name, c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Dereference) {
        ret.append(".dereference()", c, traversal);
      } else if (c.get() instanceof Expressions.ComponentName) {
        const name = c.getFirstToken().getStr().toLowerCase();
        if (name.match(/^\d/) || name.includes("/")) {
          ret.append(`["` + name + `"]`, c, traversal);
        } else {
          ret.append(`.` + name, c, traversal);
        }
      } else if (c instanceof Nodes.TokenNode) {
        const str = c.getFirstToken().getStr();
        if (str === "-") {
          ret.append(".get()", c, traversal);
        } else if (str === "->") {
          ret.append(".get().", c, traversal);
        }
      } else if (c instanceof Nodes.ExpressionNode
          && c.get() instanceof Expressions.FieldOffset) {
        extra.push("offset: " + new FieldOffsetTranspiler().transpile(c, traversal).getCode());
        this.addGetOffset = true;
      } else if (c instanceof Nodes.ExpressionNode
          && c.get() instanceof Expressions.FieldLength
          && c.concatTokens() !== "(*)") {
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