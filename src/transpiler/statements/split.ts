import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler, TargetTranspiler} from "../expressions";

export class SplitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {

    const sources = node.findDirectExpressions(abaplint.Expressions.Source);
    const source = new SourceTranspiler().transpile(sources[0]);
    const at = new SourceTranspiler().transpile(sources[1]);

    const target = new TargetTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Target)!);

    return "abap.statements.split({source: " + source + ", at: " + at + ", target: " + target + "});";
  }

}