import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler, TargetTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

const OPERANDS = ["date", "time", "stamp", "utclong", "zone", "daylight"];

export class ConvertTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const source: {[name: string]: string} = {};
    const target: {[name: string]: string} = {};

    // each operand is named by the keyword in front of it; "TIME" after "SAVING" is the
    // daylight saving flag, which may come before or after the TIME operand
    const children = node.getChildren();
    for (let i = 1; i < children.length; i++) {
      const child = children[i];
      if (!(child instanceof abaplint.Nodes.ExpressionNode)) {
        continue;
      }
      const keyword = children[i - 1].concatTokens().toUpperCase();
      const before = i >= 2 ? children[i - 2].concatTokens().toUpperCase() : "";
      const name = keyword === "TIME" && before === "SAVING" ? "daylight" : keyword.toLowerCase();
      if (OPERANDS.includes(name) === false || source[name] !== undefined || target[name] !== undefined) {
        continue;
      }

      if (child.get() instanceof abaplint.Expressions.Source) {
        source[name] = new SourceTranspiler().transpile(child, traversal).getCode();
      } else if (child.get() instanceof abaplint.Expressions.Target) {
        target[name] = new TargetTranspiler().transpile(child, traversal).getCode();
      }
    }

    const list = (operands: {[name: string]: string}) =>
      OPERANDS.filter(n => operands[n] !== undefined).map(n => n + ": " + operands[n]).join(",");

    return new Chunk(`abap.statements.convert({${list(source)}}, {${list(target)}});`);
  }

}
