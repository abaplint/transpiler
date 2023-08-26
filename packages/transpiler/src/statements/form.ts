import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";
import {IStatementTranspiler} from "./_statement_transpiler";

export class FormTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): Chunk {
    UniqueIdentifier.resetIndexBackup();
    const name = node.findFirstExpression(abaplint.Expressions.FormName)!.getFirstToken().getStr();
    return new Chunk("async function " + name + "(INPUT) {");
  }

}