import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {MethodCallTranspiler} from "./method_call";

export class MethodCallChainTranspiler implements IExpressionTranspiler {
  private readonly discardResult: boolean;

  /** @param discardResult the value of the last call in the chain is not consumed,
   *                       ie. the chain is a standalone CALL statement */
  public constructor(discardResult = false) {
    this.discardResult = discardResult;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = new Chunk();
    const children = node.getChildren();
    const calls = node.findDirectExpressions(Expressions.MethodCall);
    const lastCall = calls[calls.length - 1];

    for (const c of children) {
      if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.MethodCall) {
        const discard = this.discardResult === true && c === lastCall;
        const sub = new MethodCallTranspiler(discard).transpile(c, traversal);

        if (sub.getCode().startsWith("abap.builtin.")
            || sub.getCode().startsWith("await abap.builtin.")) {
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
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.AttributeName) {
        // an attribute between two calls, eg. "doc->get_workbook( )->worksheet->at_position( 1 )"
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
