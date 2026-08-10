import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {MethodCallTranspiler} from "./method_call";

export class MethodCallChainTranspiler implements IExpressionTranspiler {

  private static isMe(node: Nodes.ExpressionNode | Nodes.TokenNode | Nodes.StructureNode): boolean {
    return node.get() instanceof Expressions.FieldChain && node.concatTokens().toLowerCase() === "me";
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = new Chunk();
    const children = node.getChildren();

    for (const c of children) {
      if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.MethodCall) {
        const isFirst = c === node.getFirstChild();
        // "me->name( )" has the same semantics as the unqualified "name( )"
        const viaMe = children.length === 3 && c === children[2] && MethodCallChainTranspiler.isMe(children[0]);

        let prefix: string | undefined = undefined;
        if (isFirst || viaMe) {
          const nameToken = c.findDirectExpression(Expressions.MethodName)?.getFirstToken();
          if (nameToken) {
            const method = traversal.findMethodReference(nameToken, traversal.findCurrentScopeByToken(nameToken));
            prefix = traversal.constructorPrototypePrefix(nameToken, method?.def);
          }
        }

        const sub = prefix === undefined
          ? traversal.traverse(c)
          : new MethodCallTranspiler(".bind(this)").transpile(c, traversal);

        if (sub.getCode().startsWith("abap.builtin.")
            || sub.getCode().startsWith("await abap.builtin.")) {
          ret.appendChunk(sub);
        } else {
          let receiver = ret;
          let t = isFirst ? "this." : "";
          if (prefix !== undefined) {
            t = prefix;
            receiver = new Chunk(); // discard the "this.me.get()." built for the viaMe case
          }
          ret = new Chunk()
            .appendString("(await ")
            .append(t, node, traversal)
            .appendChunk(receiver)
            .appendChunk(sub)
            .appendString(")");
        }
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldChain) {
        ret.appendChunk(traversal.traverse(c));
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.NewObject) {
        ret.appendChunk(traversal.traverse(c));
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ClassName) {
        ret = new Chunk().append(traversal.lookupClassOrInterface(c.getFirstToken().getStr(), c.getFirstToken()), c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.MethodName) {
        const name = Traversal.escapeNamespace(c.getFirstToken().getStr().toLowerCase().replace("~", "$"));
        ret.append(name!, c, traversal);
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "->") {
        if (ret.getCode() === "super") {
          ret.append(".", c, traversal);
        } else {
          ret.append(".get().", c, traversal);
        }
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "=>") {
        ret.append(".", c, traversal);
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "-") {
        ret.append(".get()", c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ComponentName) {
        ret.append("." + c.concatTokens().toLowerCase(), c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Cast) {
        ret.appendChunk(traversal.traverse(c));
      } else {
        ret.append("MethodCallChainTranspilerTodo$" + c.get().constructor.name, c, traversal);
      }
    }

    const code = ret.getCode();
    if (code.startsWith("(") && code.endsWith(")")) {
      return new Chunk().append(code.substr(1, code.length - 2), node, traversal);
    } else {
      return ret;
    }
  }

}
