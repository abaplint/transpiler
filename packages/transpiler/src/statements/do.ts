import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";

export class DoTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const source = new SourceTranspiler().transpile(node.findFirstExpression(abaplint.Expressions.Source)!);
    const id = UniqueIdentifier.get();
    return "for (let " + id + " = 0; " + id + " < " + source + "; " + id + "++) {";
  }

}