import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {AttributeChainTranspiler, ComponentChainTranspiler, FieldChainTranspiler, TypeNameOrInfer} from ".";
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

    const children = node.getChildren();
    for (let i = 0; i < children.length; i++) {
      const c = children[i];
      const isLast = i === children.length - 1;

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
          ret = new Chunk().appendChunk(traversal.traverse(c)).appendString("(").appendChunk(ret).appendString(",");
          post.appendString(")");
          if (this.addGet) {
            post.append(".get()", c, traversal);
          }
        } else if (c.get() instanceof Expressions.MethodCallChain) {
          ret.appendChunk(traversal.traverse(c));
          const code = ret.getCode();
          if (code.includes("await")) {
            ret = new Chunk().appendString("(").appendChunk(ret).appendString(")");
          }
          if (this.addGet && isLast === true) {
            ret.append(".get()", c, traversal);
          }
        } else if (c.get() instanceof Expressions.Source) {
          ret.appendChunk(new SourceTranspiler(this.addGet).transpile(c, traversal));
        } else if (c.get() instanceof Expressions.Arrow) {
          ret = new Chunk().appendString("(").appendChunk(ret).appendString(").get().");
        } else if (c.get() instanceof Expressions.AttributeChain) {
          ret.appendChunk(new AttributeChainTranspiler().transpile(c, traversal));
        } else if (c.get() instanceof Expressions.ComponentChain) {
          ret = new Chunk().appendString("(").appendChunk(ret).appendString(").get().");
          ret.appendChunk(new ComponentChainTranspiler().transpile(c, traversal));
          if (this.addGet && isLast === true) {
            ret.append(".get()", c, traversal);
          }
        } else if (c.get() instanceof Expressions.Dereference) {
          ret = new Chunk().appendString("(").appendChunk(ret).appendString(").dereference()");
        } else if (c.get() instanceof Expressions.TextElement) {
          ret = new Chunk().appendString(`new abap.types.String().set("${c.concatTokens()}")`);
        } else if (c.get() instanceof Expressions.ConvBody) {
          const typ = node.findFirstExpression(Expressions.TypeNameOrInfer)
          if (typ === undefined) {
            throw new Error("TypeNameOrInfer not found in ConvBody");
          }
          ret = new Chunk().appendString(new TypeNameOrInfer().transpile(typ, traversal).getCode());
          ret.appendString(".set(");
          // todo: handle LET
          ret.appendString(traversal.traverse(c.getFirstChild()).getCode());
          ret.appendString(")");
        } else {
          ret.appendString("SourceUnknown-" + c.get().constructor.name);
        }
      } else if (c instanceof Nodes.TokenNode && (c.getFirstToken().getStr() === "&&" || c.getFirstToken().getStr() === "&")) {
        ret = new Chunk().appendString("abap.operators.concat(").appendChunk(ret).appendString(",");
        post.appendString(")");
        if (this.addGet) {
          post.append(".get()", c, traversal);
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

//    console.dir(ret.getCode());

    return ret;
  }

}