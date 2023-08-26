import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class FormTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    UniqueIdentifier.resetIndexBackup();

    const name = node.findFirstExpression(abaplint.Expressions.FormName)!.getFirstToken().getStr();

    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const def = scope?.findFormDefinition(name);

    const ret = new Chunk("async function " + name + "(INPUT) {\n");
    for (const p of def?.getChangingParameters() || []) {
      ret.appendString(`let ${p.getName()} = INPUT.${p.getName()};\n`);
    }
    for (const p of def?.getTablesParameters() || []) {
      ret.appendString(`let ${p.getName()} = INPUT.${p.getName()};\n`);
    }
    for (const p of def?.getUsingParameters() || []) {
      ret.appendString(`let ${p.getName()} = INPUT.${p.getName()};\n`);
    }
    return ret;
  }

}