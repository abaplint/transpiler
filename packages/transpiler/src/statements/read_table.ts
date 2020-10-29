import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {FieldSymbolTranspiler, SourceTranspiler} from "../expressions";

export class ReadTableTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {

    const s = node.findDirectExpression(abaplint.Expressions.BasicSource);
    const ret = traversal.traverse(s);

    const extra: string[] = [];
    const index = node.findExpressionAfterToken("INDEX");
    if (index) {
      const s = new SourceTranspiler().transpile(index, traversal);
      extra.push("index: " + s);
    }

    let pre = "";
    let post = "";
    const rt = node.findDirectExpression(abaplint.Expressions.ReadTableTarget);
    const fs = rt?.findDirectExpression(abaplint.Expressions.FSTarget);
    if (rt && fs) {
      const name = new FieldSymbolTranspiler().transpile(fs, traversal);
      pre = name + " = ";
    } else {
      const target = rt?.findDirectExpression(abaplint.Expressions.Target);
      const name = traversal.traverse(target);
      pre = name + ".set(";
      post = ")";
    }

    let concat = "";
    if (extra.length > 0) {
      concat = ",{" + extra.join(",") + "}";
    }

    return pre + "abap.statements.readTable(" + ret + concat + ")" + post + ";";
  }

}