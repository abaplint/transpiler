import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";

export class WriteTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
    let extra = "";
    const newLine = node.findDirectTokenByText("/") !== undefined;
    if (newLine === true) {
      extra = ", {newLine: true}";
    }
    const source = new SourceTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Source)!, spaghetti, filename);
    return "abap.statements.write(" + source + extra + ");";
  }

}