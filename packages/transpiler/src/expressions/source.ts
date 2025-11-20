import {AbstractType, Expressions, Nodes} from "@abaplint/core";
import {AttributeChainTranspiler, ComponentChainTranspiler, CondBodyTranspiler, FieldChainTranspiler, StringTemplateTranspiler, SwitchBodyTranspiler, TypeNameOrInfer} from ".";
import {Chunk} from "../chunk";
import {ConstantTranspiler} from "./constant";
import {CorrespondingBodyTranspiler} from "./corresponding_body";
import {FilterBodyTranspiler} from "./filter_body";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {ReduceBodyTranspiler} from "./reduce_body";
import {TranspileTypes} from "../transpile_types";
import {Traversal} from "../traversal";
import {ValueBodyTranspiler} from "./value_body";

export class SourceTranspiler implements IExpressionTranspiler {
  private readonly addGet: boolean;

  public constructor(addGet = false) {
    this.addGet = addGet;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, context?: AbstractType): Chunk {
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
          ret.appendChunk(new StringTemplateTranspiler().transpile(c, traversal, context));
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
        } else if (c.get() instanceof Expressions.TypeNameOrInfer) {
          continue;
        } else if (c.get() instanceof Expressions.ConvBody) {
          const typ = node.findFirstExpression(Expressions.TypeNameOrInfer);
          if (typ === undefined) {
            throw new Error("TypeNameOrInfer not found in ConvBody");
          }
          ret = new Chunk().appendString(new TypeNameOrInfer().transpile(typ, traversal).getCode());
          ret.appendString(".set(");
          // todo: handle LET
          const context = new TypeNameOrInfer().findType(typ, traversal);
          ret.appendString(new SourceTranspiler().transpile(c.getFirstChild() as Nodes.ExpressionNode, traversal, context).getCode());
          ret.appendString(")");
        } else if (c.get() instanceof Expressions.ValueBody) {
          continue;
        } else if (c.get() instanceof Expressions.CorrespondingBody) {
          continue;
        } else if (c.get() instanceof Expressions.CondBody) {
          continue;
        } else if (c.get() instanceof Expressions.ReduceBody) {
          continue;
        } else if (c.get() instanceof Expressions.SwitchBody) {
          continue;
        } else if (c.get() instanceof Expressions.FilterBody) {
          continue;
        } else {
          ret.appendString("SourceUnknown$" + c.get().constructor.name);
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
      } else if (c instanceof Nodes.TokenNodeRegex && c.getFirstToken().getStr().toUpperCase() === "XSDBOOL") {
        ret.append("abap.builtin.xsdbool(", c, traversal);
        post.append(")", c, traversal);
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "VALUE") {
        const typ = node.findDirectExpression(Expressions.TypeNameOrInfer);
        if (typ === undefined) {
          throw new Error("TypeNameOrInfer not found in ValueBody");
        }
        const valueBody = node.findDirectExpression(Expressions.ValueBody);
        if (valueBody) {
          ret.appendChunk(new ValueBodyTranspiler().transpile(typ, valueBody, traversal));
        } else {
          const context = new TypeNameOrInfer().findType(typ, traversal);
          ret.appendString(TranspileTypes.toType(context));
        }
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "CORRESPONDING") {
        const typ = node.findDirectExpression(Expressions.TypeNameOrInfer);
        if (typ === undefined) {
          throw new Error("TypeNameOrInfer not found in CorrespondingBody");
        }
        const correspondingBody = node.findDirectExpression(Expressions.CorrespondingBody);
        if (correspondingBody === undefined) {
          throw new Error("CorrespondingBody not found");
        }
        ret.appendChunk(new CorrespondingBodyTranspiler().transpile(typ, correspondingBody, traversal));
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "COND") {
        const typ = node.findDirectExpression(Expressions.TypeNameOrInfer);
        if (typ === undefined) {
          throw new Error("TypeNameOrInfer not found in CondBody");
        }
        const condBody = node.findDirectExpression(Expressions.CondBody);
        if (condBody === undefined) {
          throw new Error("CondBody not found");
        }
        ret.appendChunk(new CondBodyTranspiler().transpile(typ, condBody, traversal));
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "SWITCH") {
        const typ = node.findDirectExpression(Expressions.TypeNameOrInfer);
        if (typ === undefined) {
          throw new Error("TypeNameOrInfer not found in SwitchBody");
        }
        const switchBody = node.findDirectExpression(Expressions.SwitchBody);
        if (switchBody === undefined) {
          throw new Error("SwitchBody not found");
        }
        ret.appendChunk(new SwitchBodyTranspiler().transpile(typ, switchBody, traversal));
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "FILTER") {
        const typ = node.findDirectExpression(Expressions.TypeNameOrInfer);
        if (typ === undefined) {
          throw new Error("TypeNameOrInfer not found in FilterBody");
        }
        const filterBody = node.findDirectExpression(Expressions.FilterBody);
        if (filterBody === undefined) {
          throw new Error("FilterBody not found");
        }
        ret.appendChunk(new FilterBodyTranspiler().transpile(typ, filterBody, traversal));
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "REDUCE") {
        const typ = node.findDirectExpression(Expressions.TypeNameOrInfer);
        if (typ === undefined) {
          throw new Error("TypeNameOrInfer not found in ReduceBody");
        }
        const reduceBody = node.findDirectExpression(Expressions.ReduceBody);
        if (reduceBody === undefined) {
          throw new Error("ReduceBody not found");
        }
        ret.appendChunk(new ReduceBodyTranspiler().transpile(typ, reduceBody, traversal));
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "REF") {
        const infer = node.findDirectExpression(Expressions.TypeNameOrInfer);
        if (infer?.concatTokens() !== "#") {
          throw new Error("transpiler: REF # todo1");
        }
        const scope = traversal.findCurrentScopeByToken(infer.getFirstToken());
        const inferType = traversal.lookupInferred(infer, scope);
        if (inferType === undefined) {
          throw new Error("transpiler: REF # todo, lookupInferred, " + node.concatTokens());
        }
        const typ = TranspileTypes.toType(inferType);
        if (typ.startsWith("new abap.types.DataReference(") === false) {
          throw new Error("transpiler: REF # unexpected type");
        }
        ret.append(`abap.statements.getReference(${typ}, `, c, traversal);
        post.append(")", c, traversal);
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "BIT") { // todo, this will not work in the general case
        ret.append("abap.operators.bitnot(", c, traversal);
        post.append(")", c, traversal);
      }
    }

    ret.appendChunk(post);

//    console.dir("return: " + ret.getCode());

    return ret;
  }

}