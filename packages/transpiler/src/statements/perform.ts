/* eslint-disable max-len */
import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class PerformTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const formName = node.findDirectExpression(abaplint.Expressions.FormName);
    if (formName === undefined) {
      return new Chunk(`throw new Error("PerformTranspiler FormName not found");`);
    } else if (node.concatTokens().toUpperCase().includes(" IN PROGRAM ")) {
// todo: throw exception if not found?
      const expression = node.findExpressionAfterToken("PROGRAM");
      let ref = "";
      if (expression?.get() instanceof abaplint.Expressions.Dynamic) {
        const name = expression.getChildren()[1].concatTokens() + ".get().trimEnd()";
        ref = `abap.Forms['PROG-' + ${name} + '-${formName.concatTokens().toUpperCase()}']`;
      } else {
        const progName = expression?.concatTokens().toUpperCase();
        ref = `abap.Forms['PROG-${progName}-${formName.concatTokens().toUpperCase()}']`;
      }
      const params: string[] = [];
// hacky hack
      for (const t of node.findDirectExpression(abaplint.Expressions.PerformChanging)?.findDirectExpressions(abaplint.Expressions.Target) || []) {
        const name = t.getFirstToken().getStr();
        params.push(`"${name}": ` + traversal.traverse(t).getCode());
      }

      let call = "await " + ref + `({${params.join(",")}});`;
      if (node.concatTokens().toUpperCase().includes(" IF FOUND")) {
        call = `if (${ref} !== undefined) { ${call} }`;
      }

      return new Chunk(call);
    } else {
// todo: most of this needs rewriting?
      let def: abaplint.Types.FormDefinition | undefined = undefined;

      const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
      for (const r of scope?.getData().references || []) {
        if (r.referenceType === abaplint.ReferenceType.FormReference
            && r.position.getStart().equals(formName.getFirstToken().getStart())
            && r.resolved instanceof abaplint.Types.FormDefinition) {
          def = r.resolved;
        }
      }

// todo: pass by VALUE()

      const params: string[] = [];

      let index = 0;
      for (const t of node.findDirectExpression(abaplint.Expressions.PerformTables)?.findDirectExpressions(abaplint.Expressions.Source) || []) {
        const name = def?.getTablesParameters()[index].getName().toLowerCase();
        if (name === undefined) {
          continue;
        }
        params.push(`"${name}": ` + traversal.traverse(t).getCode());
        index++;
      }

      index = 0;
      for (const u of node.findDirectExpression(abaplint.Expressions.PerformUsing)?.findDirectExpressions(abaplint.Expressions.Source) || []) {
        const name = def?.getUsingParameters()[index].getName().toLowerCase();
        if (name === undefined) {
          continue;
        }
        params.push(`"${name}": ` + traversal.traverse(u).getCode());
        index++;
      }

      index = 0;
      for (const c of node.findDirectExpression(abaplint.Expressions.PerformChanging)?.findDirectExpressions(abaplint.Expressions.Source) || []) {
        const name = def?.getChangingParameters()[index].getName().toLowerCase();
        if (name === undefined) {
          continue;
        }
        params.push(`"${name}": ` + traversal.traverse(c).getCode());
        index++;
      }

      return new Chunk("await " + formName.concatTokens() + `({${params.join(",")}});`);
    }
  }

}