import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler, TargetTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ConvertTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
//    const source = new SourceTranspiler(true).transpile(node.findDirectExpression(abaplint.Expressions.Source)!, traversal);

    const source: string[] = [];
    const target: string[] = [];

    const date = node.findExpressionAfterToken("DATE");
    if (date?.get() instanceof abaplint.Expressions.Source) {
      source.push("date: " + new SourceTranspiler().transpile(date, traversal).getCode());
    } else if (date?.get() instanceof abaplint.Expressions.Target) {
      target.push("date: " + new TargetTranspiler().transpile(date, traversal).getCode());
    }

    const time = node.findExpressionAfterToken("TIME");
    if (time?.get() instanceof abaplint.Expressions.Source) {
      source.push("time: " + new SourceTranspiler().transpile(time, traversal).getCode());
    } else if (time?.get() instanceof abaplint.Expressions.Target) {
      target.push("time: " + new TargetTranspiler().transpile(time, traversal).getCode());
    }

    const stamp = node.findExpressionAfterToken("STAMP");
    if (stamp?.get() instanceof abaplint.Expressions.Source) {
      source.push("stamp: " + new SourceTranspiler().transpile(stamp, traversal).getCode());
    } else if (stamp?.get() instanceof abaplint.Expressions.Target) {
      target.push("stamp: " + new TargetTranspiler().transpile(stamp, traversal).getCode());
    }

    const zone = node.findExpressionAfterToken("ZONE");
    if (zone?.get() instanceof abaplint.Expressions.Source) {
      source.push("zone: " + new SourceTranspiler().transpile(zone, traversal).getCode());
    } else if (zone?.get() instanceof abaplint.Expressions.Target) {
      target.push("zone: " + new TargetTranspiler().transpile(zone, traversal).getCode());
    }

    return new Chunk(`abap.statements.convert({${source.join(",")}}, {${target.join(",")}});`);
  }

}