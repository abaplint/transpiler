import {Expressions, Nodes} from "abaplint";
import * as abaplint from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {FieldChainTranspiler, ArithOperatorTranspiler, ConstantTranspiler, MethodCallChainTranspiler, StringTemplateTranspiler} from ".";

export class SourceTranspiler implements IExpressionTranspiler {
  private readonly addGet: boolean;

  public constructor(addGet = false) {
    this.addGet = addGet;
  }

  public transpile(node: Nodes.ExpressionNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
    let ret = "";
    let post = "";

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        if (c.get() instanceof Expressions.FieldChain) {
          ret = ret + new FieldChainTranspiler(this.addGet).transpile(c, spaghetti, filename);
        } else if (c.get() instanceof Expressions.Constant) {
          ret = ret + new ConstantTranspiler().transpile(c);
        } else if (c.get() instanceof Expressions.StringTemplate) {
          ret = ret + new StringTemplateTranspiler().transpile(c, spaghetti, filename);
        } else if (c.get() instanceof Expressions.ArithOperator) {
          ret = ret + new ArithOperatorTranspiler().transpile(c);
          post = ")";
        } else if (c.get() instanceof Expressions.MethodCallChain) {
          ret = ret + new MethodCallChainTranspiler().transpile(c, spaghetti, filename);
          if (this.addGet) {
            ret = ret + ".get()";  // todo, this will break
          }
        } else if (c.get() instanceof Expressions.Source) {
          ret = ret + this.transpile(c, spaghetti, filename);
        } else {
          ret = ret + "Source, unknown";
        }
      }
    }

    return ret + post;
  }

}