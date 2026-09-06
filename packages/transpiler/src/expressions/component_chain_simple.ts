import {Nodes, Expressions} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {FieldLengthTranspiler} from "./field_length";
import {FieldOffsetTranspiler} from "./field_offset";

export class ComponentChainSimpleTranspiler implements IExpressionTranspiler {
  private readonly prefix: string = "";

  public constructor(prefix = "") {
    this.prefix = prefix;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const offset: string[] = [];
    let ret = new Chunk();
    let prefix = this.prefix;

    for (const c of node.getChildren()) {
      const type = c.get();
      if (type instanceof Expressions.ComponentName) {
        let field = c.getFirstToken().getStr().toLowerCase();

        const interfaceName = traversal.isInterfaceAttribute(c.getFirstToken());
        if (interfaceName && field.startsWith(interfaceName) === false) {
          field = interfaceName + "$" + field;
        }
        field = Traversal.escapeNamespace(field)!.replace("~", "$");
        if (Traversal.isBracketComponent(field)) {
          ret.append(prefix.replace(/\.$/, "") + `["${field}"]`, c, traversal);
        } else {
          ret.append(prefix + field, c, traversal);
        }
        prefix = "";
      } else if (type instanceof Expressions.ArrowOrDash) {
        ret.append(".get()", c, traversal);
        prefix = ".";
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldOffset) {
        offset.push("offset: " + new FieldOffsetTranspiler().transpile(c, traversal).getCode());
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldLength) {
        const len = new FieldLengthTranspiler().transpile(c, traversal).getCode();
        if (len !== "*") {
          offset.push("length: " + len);
        }
      }
    }

    if (offset.length > 0) {
      const pre = "new abap.OffsetLength(";
      const post = ", {" + offset.join(", ") + "})";
      ret = new Chunk().appendString(pre).appendChunk(ret).appendString(post);
    }

    return ret;
  }

}
