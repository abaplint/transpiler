import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler, TargetTranspiler} from "../expressions";

export class AppendTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
    const source = new SourceTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.SimpleSource)!, spaghetti, filename);
    const target = new TargetTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Target)!);

    return "abap.statements.append({source: " + source + ", target: " + target + "});";
  }

}