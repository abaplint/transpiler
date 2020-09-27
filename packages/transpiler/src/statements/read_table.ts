import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {FieldSymbolTranspiler, SourceTranspiler} from "../expressions";

export class ReadTableTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {

    let ret = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));
    ret += ".array()";

    const index = node.findExpressionAfterToken("INDEX");
    if (index) {
      const s = new SourceTranspiler(true).transpile(index, traversal);
      ret += "[" + s + " - 1]";
    }

    const rt = node.findDirectExpression(abaplint.Expressions.ReadTableTarget);
    const fs = rt?.findDirectExpression(abaplint.Expressions.FSTarget);
    if (rt && fs) {
      const name = new FieldSymbolTranspiler().transpile(fs, traversal);
      ret = name + " = " + ret + ";";
    } else {
      const target = rt?.findDirectExpression(abaplint.Expressions.Target);
      const name = traversal.traverse(target);
      ret = name + ".set(" + ret + ".get());";
    }

    return ret;
  }

}