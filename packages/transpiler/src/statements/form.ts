import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class FormTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    UniqueIdentifier.resetIndexBackup();

    const formName = node.findFirstExpression(abaplint.Expressions.FormName)!;

    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const def = scope?.findFormDefinition(formName.concatTokens());

    const ret = new Chunk("async function " + Traversal.formName(formName) + "(INPUT) {");
    const params: string[] = [];
    for (const p of def?.getChangingParameters() || []) {
      params.push(`let ${Traversal.prefixVariable(p.getName())} = INPUT.${p.getName()};`);
    }
    for (const p of def?.getTablesParameters() || []) {
      params.push(`let ${Traversal.prefixVariable(p.getName())} = INPUT.${p.getName()};`);
    }
    for (const p of def?.getUsingParameters() || []) {
      params.push(`let ${Traversal.prefixVariable(p.getName())} = INPUT.${p.getName()};`);
    }
    if (params.length > 0) {
      ret.appendString("\n" + params.join("\n"));
    }
    return ret;
  }

}