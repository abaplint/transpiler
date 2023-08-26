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
      return new Chunk(`throw new Error("PerformTranspiler IN PROGRAM, transpiler todo");`);
    }

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

    const params: {[key: string]: string;} = {};

    let index = 0;
    for (const t of node.findDirectExpression(abaplint.Expressions.PerformTables)?.findDirectExpressions(abaplint.Expressions.Source) || []) {
      console.dir(t);
      const name = def?.getTablesParameters()[index].getName().toLowerCase();
      if (name === undefined) {
        continue;
      }
      params[name] = traversal.traverse(t).getCode();
      index++;
    }

    index = 0;
    for (const u of node.findDirectExpression(abaplint.Expressions.PerformUsing)?.findDirectExpressions(abaplint.Expressions.Source) || []) {
      console.dir(u);
      const name = def?.getUsingParameters()[index].getName().toLowerCase();
      if (name === undefined) {
        continue;
      }
      params[name] = traversal.traverse(u).getCode();
      index++;
    }

    index = 0;
    for (const c of node.findDirectExpression(abaplint.Expressions.PerformChanging)?.findDirectExpressions(abaplint.Expressions.Source) || []) {
      console.dir(c);
      const name = def?.getChangingParameters()[index].getName().toLowerCase();
      if (name === undefined) {
        continue;
      }
      params[name] = traversal.traverse(c).getCode();
      index++;
    }

    return new Chunk("await " + formName.concatTokens() + `(${JSON.stringify(params)});`);
  }

}