import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler, TargetTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";

export class LoopTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const source = new SourceTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Source)!);
    const target = new TargetTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Target)!);

    const unique = UniqueIdentifier.get();
    return "for (let " + unique + " of " + source + ".array()) {\n" +
      target + ".set(" + unique + ");";
  }

}