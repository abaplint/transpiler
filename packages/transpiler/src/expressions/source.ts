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
    const post = new Chunk();

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
          const code = ret.copy();
          ret = new Chunk().appendChunk(traversal.traverse(c));
          ret.appendString("(").appendChunk(code).appendString(",");
          post.appendString(")");
          if (this.addGet) {
            post.appendString(".get()");
          }
        } else if (c.get() instanceof Expressions.MethodCallChain) {
          ret.appendChunk(traversal.traverse(c));
          if (this.addGet) {
            const code = ret.getCode();
            if (code.includes("await")) {
              ret = new Chunk().appendString("(").appendChunk(ret).appendString(").get()");
            } else {
              ret.appendString(".get()");
            }
          }
        } else if (c.get() instanceof Expressions.Source) {
          ret.appendChunk(new SourceTranspiler(this.addGet).transpile(c, traversal));
        } else if (c.get() instanceof Expressions.Arrow) {
          const code = ret.copy();
          ret = new Chunk();
          ret.appendString("(").appendChunk(code).appendString(").get().");
        } else if (c.get() instanceof Expressions.AttributeChain) {
          ret.appendChunk(new AttributeChainTranspiler().transpile(c, traversal));
        } else if (c.get() instanceof Expressions.ComponentChain) {
          const code = ret.copy();
          ret = new Chunk();
          ret.appendString("(").appendChunk(code).appendString(").get().");
          ret.appendChunk(new ComponentChainTranspiler().transpile(c, traversal));
        } else if (c.get() instanceof Expressions.Dereference) {
          const code = ret.copy();
          ret = new Chunk();
          ret.appendString("(").appendChunk(code).appendString(").getPointer()");
        } else {
          ret.appendString("SourceUnknown-" + c.get().constructor.name);
        }
      } else if (c instanceof Nodes.TokenNode && (c.getFirstToken().getStr() === "&&" || c.getFirstToken().getStr() === "&")) {
        if (this.addGet === false) {
          return new SourceTranspiler(true).transpile(node, traversal);
        } else {
          ret.append(" + ", c, traversal);
        }
      } else if (c instanceof Nodes.TokenNodeRegex && c.getFirstToken().getStr().toUpperCase() === "BOOLC") {
        ret.append("abap.builtin.boolc(", c, traversal);
        post.append(")", c, traversal);
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "BIT") { // todo, this will not work in the general case
        ret.append("abap.operators.bitnot(", c, traversal);
        post.append(")", c, traversal);
      }
    }

    ret.appendChunk(post);

    return ret;
  }

}