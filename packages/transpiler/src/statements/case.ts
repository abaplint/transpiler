import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";

export class CaseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const source = new SourceTranspiler(true).transpile(node.findDirectExpression(abaplint.Expressions.Source)!);
    return "switch (" + source + ") {";
  }

}