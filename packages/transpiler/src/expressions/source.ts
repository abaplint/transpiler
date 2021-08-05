import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {AttributeChainTranspiler, ComponentChainTranspiler, FieldChainTranspiler} from ".";
import {Traversal} from "../traversal";
import {ConstantTranspiler} from "./constant";
import {Chunk} from "../chunk";

export class SourceTranspiler implements IExpressionTranspiler {
  private readonly addGet: boolean;

  public constructor(addGet = false) {
    this.addGet = addGet;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = new Chunk();
    let post = "";

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        if (c.get() instanceof Expressions.FieldChain) {
          ret.appendChunk(new FieldChainTranspiler(this.addGet).transpile(c, traversal));
        } else if (c.get() instanceof Expressions.Constant) {
          ret.appendChunk(new ConstantTranspiler(this.addGet).transpile(c, traversal));
        } else if (c.get() instanceof Expressions.StringTemplate) {
          ret.appendChunk(traversal.traverse(c));
        } else if (c.get() instanceof Expressions.Cond) {
          ret.appendChunk(traversal.traverse(c));
        } else if (c.get() instanceof Expressions.ArithOperator) {
          const code = ret.getCode();
          ret = new Chunk();
          ret.appendChunk(traversal.traverse(c));
          ret.appendString("(" + code + ",");
          post = ")";
          if (this.addGet) {
            post += ".get()";
          }
        } else if (c.get() instanceof Expressions.MethodCallChain) {
          ret.appendChunk(traversal.traverse(c));
          const code = ret.getCode();
          if (this.addGet) {
            if (ret.getCode().includes("await")) {
              ret = new Chunk();
              ret.appendString("(" + code + ").get()");
            } else {
              ret.appendString(".get()");
            }
          }
        } else if (c.get() instanceof Expressions.Source) {
          ret.appendChunk(new SourceTranspiler(this.addGet).transpile(c, traversal));
        } else if (c.get() instanceof Expressions.Arrow) {
          const code = ret.getCode();
          ret = new Chunk();
          ret.appendString("(" + code + ").get().");
        } else if (c.get() instanceof Expressions.AttributeChain) {
          ret.appendChunk(new AttributeChainTranspiler().transpile(c, traversal));
        } else if (c.get() instanceof Expressions.ComponentChain) {
          const code = ret.getCode();
          ret = new Chunk();
          ret.appendString("(" + code + ").get().");
          ret.appendChunk(new ComponentChainTranspiler().transpile(c, traversal));
        } else if (c.get() instanceof Expressions.Dereference) {
          const code = ret.getCode();
          ret = new Chunk();
          ret.appendString("(" + code + ").getPointer()");
        } else {
          ret.appendString("SourceUnknown-" + c.get().constructor.name);
        }
      } else if (c instanceof Nodes.TokenNode && (c.getFirstToken().getStr() === "&&" || c.getFirstToken().getStr() === "&")) {
        if (this.addGet === false) {
          return new SourceTranspiler(true).transpile(node, traversal);
        } else {
          ret.appendString(" + ");
        }
      } else if (c instanceof Nodes.TokenNodeRegex && c.getFirstToken().getStr().toUpperCase() === "BOOLC") {
        ret.appendString("abap.builtin.boolc(");
        post += ")";
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "BIT") { // todo, this will not work in the general case
        ret.appendString("abap.operators.bitnot(");
        post += ")";
      }
    }

    ret.appendString(post);

    return ret;
  }

}