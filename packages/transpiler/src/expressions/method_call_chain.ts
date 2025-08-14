import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class MethodCallChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = new Chunk();

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.MethodCall) {
        const sub = traversal.traverse(c);
        if (sub.getCode().startsWith("abap.builtin.")) {
          ret.appendChunk(sub);
        } else {
          const t = c === node.getFirstChild() ? "this." : "";
          ret = new Chunk()
            .appendString("(await ")
            .append(t, node, traversal)
            .appendChunk(ret)
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