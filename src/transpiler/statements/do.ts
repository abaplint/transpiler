import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";

export class DoTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const source = new SourceTranspiler().transpile(node.findFirstExpression(abaplint.Expressions.Source)!);
// todo, "i" must be unique
    return "for (let i = 0; i < " + source + "; i++) {";
  }

}