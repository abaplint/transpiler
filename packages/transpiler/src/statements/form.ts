import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {IStatementTranspiler} from "./_statement_transpiler";

export class FormTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): Chunk {
    const name = node.findFirstExpression(abaplint.Expressions.FormName)!.getFirstToken().getStr();
    return new Chunk("async function " + name + "() {");
  }

}