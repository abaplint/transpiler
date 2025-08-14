import {Expressions, Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {FieldLengthTranspiler, FieldOffsetTranspiler, FieldSymbolTranspiler, TableExpressionTranspiler} from ".";
import {Chunk} from "../chunk";
import {FEATURE_FLAGS} from "../feature_flags";

export class TargetTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const offset: string[] = [];
    let ret = new Chunk();
    let context: abaplint.AbstractType | undefined = undefined;
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());

    const children = node.getChildren();
    for (let i = 0; i < children.length; i++) {
      const c = children[i];
      const next: Nodes.ExpressionNode | Nodes.TokenNode | undefined = children[i + 1];

      if (c.get() instanceof Expressions.TargetField) {
        const prefix = traversal.prefixAndName(c.getFirstToken()).replace("~", "$");
        ret.append(Traversal.prefixVariable(Traversal.escapeNamespace(prefix)!), c, traversal);

        context = scope?.findVariable(c.getFirstToken().getStr())?.getType();
      } else if (c.get() instanceof Expressions.TableExpression && c instanceof Nodes.ExpressionNode) {
        ret = new TableExpressionTranspiler().transpile(c, traversal, ret);

      } else if (c.get() instanceof Expressions.InlineData && c instanceof Nodes.ExpressionNode) {
        const targetField = c.findDirectExpression(Expressions.TargetField);
        if (targetField === undefined) {
          throw new Error("TargetTranspiler: InlineData Target field not found");
        }
        ret.append(Traversal.prefixVariable(Traversal.escapeNamespace(targetField?.concatTokens())!), c, traversal);
        context = scope?.findVariable(targetField!.getFirstToken().getStr())?.getType();
      } else if (c.get() instanceof Expressions.ClassName) {
        const name = traversal.lookupClassOrInterface(c.getFirstToken().getStr(), c.getFirstToken());
        ret.append(name, c, traversal);
      } else if (c.get() instanceof Expressions.ComponentName) {
        const name = c.getFirstToken().getStr().toLowerCase();
        if (name.match(/^\d/) || name.includes("/")) {
          ret.append(`["` + name + `"]`, c, traversal);
        } else {
          ret.append(`.` + name, c, traversal);
        }
      } else if (c.get() instanceof Expressions.AttributeName) {
        let prefix = "";
        let postfix = ""
        if (context instanceof abaplint.BasicTypes.ObjectReferenceType) {
          const cdef = traversal.findClassDefinition(context.getIdentifierName(), scope);
          const attr = cdef?.getAttributes().findByName(c.getFirstToken().getStr());
          if (FEATURE_FLAGS.PRIVATE_ATTRIBUTES === true
              && attr?.getVisibility() === abaplint.Visibility.Private) {
            const id = scope?.getParent()?.getParent()?.getIdentifier();
            if (id?.stype === abaplint.ScopeType.ClassImplementation
                && cdef?.getName().toUpperCase() === id.sname.toUpperCase()) {
              prefix = "#";
            } else {
              prefix = `FRIENDS_ACCESS_INSTANCE["`;
              postfix = `"]`;
            }
          }
        }
        const intf = Traversal.escapeNamespace(traversal.isInterfaceAttribute(c.getFirstToken()));
        let name = prefix + Traversal.escapeNamespace(c.getFirstToken().getStr())!.replace("~", "$").toLowerCase() + postfix;
        if (intf && name.startsWith(intf) === false) {
          name = intf + "$" + name;
        }
        ret.append(name, c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldOffset) {
        offset.push("offset: " + new FieldOffsetTranspiler().transpile(c, traversal).getCode());
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldLength) {
        const len = new FieldLengthTranspiler().transpile(c, traversal).getCode();
        if (len !== "*") {
          offset.push("length: " + len);
        }
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.TargetFieldSymbol) {
        ret.appendChunk(new FieldSymbolTranspiler().transpile(c, traversal));
      } else if (c.getFirstToken().getStr() === "-") {
        ret.append(".get()", c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Dereference) {
        ret.append(".dereference()", c, traversal);
      } else if (c.getFirstToken().getStr() === "=>") {
        ret.append(".", c, traversal);
      } else if (c.getFirstToken().getStr() === "->") {
        if (next.concatTokens() === "*") {
          ret.append(".dereference()", c, traversal);
        } else {
          ret.append(".get().", c, traversal);
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