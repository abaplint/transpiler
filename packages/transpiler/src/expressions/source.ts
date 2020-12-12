import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {FieldChainTranspiler} from ".";
import {Traversal} from "../traversal";
import {ConstantTranspiler} from "./constant";

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
          ret += new FieldChainTranspiler(this.addGet).transpile(c, traversal);
        } else if (c.get() instanceof Expressions.Constant) {
          ret += new ConstantTranspiler(this.addGet).transpile(c, traversal);
        } else if (c.get() instanceof Expressions.StringTemplate) {
          ret += traversal.traverse(c);
        } else if (c.get() instanceof Expressions.Cond) {
          ret += traversal.traverse(c);
        } else if (c.get() instanceof Expressions.ArithOperator) {
          ret = traversal.traverse(c) + "(" + ret + ",";
          post = ")";
          if (this.addGet) {
            post += ".get()";
          }
        } else if (c.get() instanceof Expressions.MethodCallChain) {
          ret += traversal.traverse(c);
          if (this.addGet) {
            ret += ".get()";
          }
        } else if (c.get() instanceof Expressions.Source) {
          ret += new SourceTranspiler(this.addGet).transpile(c, traversal);
        } else {
          ret += "SourceUnknown-" + c.get().constructor.name;
        }
      } else if (c instanceof Nodes.TokenNode && (c.getFirstToken().getStr() === "&&" || c.getFirstToken().getStr() === "&")) {
        if (this.addGet === false) {
          return new SourceTranspiler(true).transpile(node, traversal);
        } else {
          ret += " + ";
        }
      } else if (c instanceof Nodes.TokenNodeRegex && c.getFirstToken().getStr().toUpperCase() === "BOOLC") {
        ret += "abap.builtin.boolc(";
        post += ")";
      }
    }

    ret = ret + post;

    return ret;
  }

}