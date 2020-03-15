import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {FieldChainTranspiler} from ".";
import {Traversal} from "../traversal";

export class SourceTranspiler implements IExpressionTranspiler {
  private readonly addGet: boolean;

  public constructor(addGet = false) {
    this.addGet = addGet;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";
    let post = "";

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        if (c.get() instanceof Expressions.FieldChain) {
          ret = ret + new FieldChainTranspiler(this.addGet).transpile(c, traversal);
        } else if (c.get() instanceof Expressions.Constant) {
          ret = ret + traversal.traverse(c);
        } else if (c.get() instanceof Expressions.StringTemplate) {
          ret = ret + traversal.traverse(c);
        } else if (c.get() instanceof Expressions.ArithOperator) {
          ret = ret + traversal.traverse(c);
          post = ")";
        } else if (c.get() instanceof Expressions.MethodCallChain) {
          ret = ret + traversal.traverse(c);
          if (this.addGet) {
            ret = ret + ".get()";  // todo, this will break
          }
        } else if (c.get() instanceof Expressions.Source) {
          ret = ret + this.transpile(c, traversal);
        } else {
          ret = ret + "Source, unknown";
        }
      }
    }

    return ret + post;
  }

}