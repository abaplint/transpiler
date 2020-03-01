import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler, TargetTranspiler} from "../expressions";

export class LoopTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const source = new SourceTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Source)!);
    const target = new TargetTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Target)!);

    return "for (" + target + " of " + source + ".array()) {";
  }

}