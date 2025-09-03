import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";
import {SourceTranspiler} from "./source";

export class SwitchBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("SwitchBodyTranspiler, Expected TypeNameOrInfer");
    } else if (body.findDirectExpression(Expressions.Let)) {
      throw new Error("SwitchBodyTranspiler, Let not supported, todo");
    }

    type whenThenType = {whenOr: Nodes.ExpressionNode[], then: Nodes.ExpressionNode | undefined};
    const source = traversal.traverse(body.findDirectExpression(Expressions.Source));
    const whenThenOr: whenThenType[] = [];
    const elseExpression = body.findExpressionAfterToken("ELSE");

    {
      let currentWhenThen: whenThenType = {whenOr: [], then: undefined};
      let mode: "WHEN" | "THEN" | "" = "";
      for (const c of body.getChildren()) {
        if (c instanceof Nodes.TokenNode) {
          if (c.concatTokens() === "WHEN") {
            if (currentWhenThen.whenOr.length > 0) {
              whenThenOr.push(currentWhenThen);
            }
            currentWhenThen = {whenOr: [], then: undefined};
            mode = "WHEN";
          } else if (c.concatTokens() === "THEN") {
            mode = "THEN";
          }
        } else if (mode === "WHEN" && c instanceof Nodes.ExpressionNode) {
          currentWhenThen.whenOr.push(c);
        } else if (mode === "THEN" && c instanceof Nodes.ExpressionNode) {
          currentWhenThen.then = c;
        }
      }
      if (currentWhenThen.whenOr.length > 0) {
        whenThenOr.push(currentWhenThen);
      }
    }

    const type = new TypeNameOrInfer().findType(typ, traversal);
    const target = TranspileTypes.toType(type);

    const ret = new Chunk();
    ret.appendString("(" + target + ".set(");
    ret.appendString("await (async () => {\n");

    for (const wto of whenThenOr) {
      let thenValue = "";
      if (wto.then?.get() instanceof Expressions.Source) {
        thenValue = new SourceTranspiler().transpile(wto.then, traversal).getCode();
      } else {
        throw new Error("SwitchBodyTranspiler, Expected Source1, todo, " + wto.then?.get().constructor.name);
      }

      // todo, async await?
      for (const thenOr of wto.whenOr) {
        let condition = "";
        if (thenOr.get() instanceof Expressions.Source) {
          condition = new SourceTranspiler().transpile(thenOr, traversal).getCode();
        } else {
          throw new Error("SwitchBodyTranspiler, Expected Source2, todo, " + thenOr.get().constructor.name);
        }

        ret.appendString(`if (abap.compare.eq(${source.getCode()}, ${condition})) { return ${thenValue}; }\n`);
      }
    }

    if (elseExpression) {
      if (!(elseExpression.get() instanceof Expressions.Source)) {
        throw new Error("SwitchBodyTranspiler, Expected Source3, todo, " + elseExpression.get().constructor.name);
      }
      const value = new SourceTranspiler().transpile(elseExpression, traversal).getCode();
      ret.appendString(`return ${value};\n`);
    } else {
      ret.appendString(`return ${target};\n`);
    }

    ret.appendString("})()))");
    return ret;
  }

}