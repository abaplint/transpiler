import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";
import {CondTranspiler} from "./cond";
import {SourceTranspiler} from "./source";

export class SwitchBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("SwitchBodyTranspiler, Expected TypeNameOrInfer");
    } else if (body.findDirectExpression(Expressions.Let)) {
      throw new Error("SwitchBodyTranspiler, Let not supported, todo");
    }

    type whenThenType = {whenOr: Nodes.ExpressionNode[], then: Nodes.ExpressionNode | undefined};
    const source = body.findDirectExpression(Expressions.Source);
    const whenThen: whenThenType[] = [];
    const elseExpression = body.findExpressionAfterToken("ELSE");

    let currentWhenThen: whenThenType = {whenOr: [], then: undefined};
    let mode: "WHEN" | "THEN" = "WHEN";
    for (const c of body.getChildren()) {
      if (c instanceof Nodes.TokenNode) {
        if (c.concatTokens() === "WHEN") {
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

/*
    const type = new TypeNameOrInfer().findType(typ, traversal);
    const target = TranspileTypes.toType(type);

    const ret = new Chunk();
    ret.appendString("(" + target + ".set(");
    ret.appendString("await (async () => {\n");

    for (const {when, then} of whenThen) {
      let condition = "";
      if (when.get() instanceof Expressions.Cond) {
        condition = new CondTranspiler().transpile(when, traversal).getCode();
      } else {
        throw new Error("CondBodyTranspiler, Expected Cond, todo, " + when.get().constructor.name);
      }

      let value = "";
      if (then.get() instanceof Expressions.Source) {
        value = new SourceTranspiler().transpile(then, traversal).getCode();
      } else {
        throw new Error("CondBodyTranspiler, Expected Source, todo, " + then.get().constructor.name);
      }

      ret.appendString(`if (${condition}) { return ${value}; }\n`);
    }

    const els = body.findExpressionAfterToken("ELSE");
    if (els) {
      if (!(els.get() instanceof Expressions.Source)) {
        throw new Error("CondBodyTranspiler, Expected Source, todo, " + els.get().constructor.name);
      }
      const value = new SourceTranspiler().transpile(els, traversal).getCode();
      ret.appendString(`return ${value};\n`);
    } else {
      ret.appendString(`return ${target};\n`);
    }

    ret.appendString("})()))");
    return ret;
    */
  }

}