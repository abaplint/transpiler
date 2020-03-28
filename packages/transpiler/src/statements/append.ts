import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class AppendTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const source = new SourceTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.SimpleSource)!, traversal);
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    return "abap.statements.append({source: " + source + ", target: " + target + "});";
  }

}