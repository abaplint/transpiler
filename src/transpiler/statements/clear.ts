import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TargetTranspiler} from "../expressions";

export class ClearTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const target = new TargetTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Target)!);
    return target + ".clear();";
  }

}