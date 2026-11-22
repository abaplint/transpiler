/* eslint-disable max-len */
import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class PerformTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const formName = node.findDirectExpression(abaplint.Expressions.FormName);
    // PERFORM (name) IN PROGRAM ...: the name of the subroutine is the content of the field, as it is, so it must be
    // in upper case to be found
    const dynamicName = formName === undefined ? node.findDirectExpression(abaplint.Expressions.Dynamic) : undefined;
    if (formName === undefined && dynamicName === undefined) {
      return new Chunk(`throw new Error("PerformTranspiler FormName not found");`);
    } else if (formName === undefined || node.concatTokens().toUpperCase().includes(" IN PROGRAM ")) {
      // the end of the key in abap.Forms, after an open literal 'PROG-<program>-
      const formKey = formName === undefined
        ? "' + " + traversal.traverse(dynamicName!.getChildren()[1]).getCode() + ".get().toString().trimEnd()"
        : formName.concatTokens().toUpperCase() + "'";
      const expression = node.findExpressionAfterToken("PROGRAM");
      let ref = "";
      let program = "";
      if (expression?.get() instanceof abaplint.Expressions.Dynamic) {
        // any field, eg. a structure component; sy-repid is the name of the current program
        const field = expression.getChildren()[1];
        const name = field.concatTokens().toUpperCase() === "SY-REPID"
          ? `'${traversal.getCurrentObject().getName().toUpperCase()}'`
          : traversal.traverse(field).getCode() + ".get().trimEnd()";
        ref = `abap.Forms['PROG-' + ${name} + '-${formKey}]`;
        program = `'PROG-' + ${name} + '-'`;
      } else {
        // IN PROGRAM without a name, or no IN PROGRAM: the current program
        const progName = expression?.concatTokens().toUpperCase() ?? traversal.getCurrentObject().getName().toUpperCase();
        ref = `abap.Forms['PROG-${progName}-${formKey}]`;
        program = `'PROG-${progName}-'`;
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
      } else {
        // the form is missing from a program whose forms are registered: CX_SY_DYN_CALL_ILLEGAL_FORM,
        // no form of the program is registered: CX_SY_PROGRAM_NOT_FOUND
        const raise = (name: string) => {
          const cls = traversal.lookupClassOrInterface(`'${name}'`, node.getFirstToken(), true);
          return `if (${cls} === undefined) { throw "${name} not found"; } else { throw await new ${cls}().constructor_(); }`;
        };
        const loaded = `Object.keys(abap.Forms).some(k => k.startsWith(${program}))`;
        call = `if (${ref} === undefined) { if (${loaded}) { ${raise("CX_SY_DYN_CALL_ILLEGAL_FORM")} } else { ${raise("CX_SY_PROGRAM_NOT_FOUND")} } }\n` + call;
      }

      return PerformTranspiler.onCommit(node, traversal, call);
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
      for (const u of node.findDirectExpression(abaplint.Expressions.PerformUsing)?.findDirectExpressions(abaplint.Expressions.SimpleSource3) || []) {
        const name = def?.getUsingParameters()[index].getName().toLowerCase();
        if (name === undefined) {
          continue;
        }
        params.push(`"${name}": ` + traversal.traverse(u).getCode());
        index++;
      }

      index = 0;
      for (const c of node.findDirectExpression(abaplint.Expressions.PerformChanging)?.findDirectExpressions(abaplint.Expressions.Target) || []) {
        const name = def?.getChangingParameters()[index].getName().toLowerCase();
        if (name === undefined) {
          continue;
        }
        params.push(`"${name}": ` + traversal.traverse(c).getCode());
        index++;
      }

      return PerformTranspiler.onCommit(node, traversal, "await " + Traversal.formName(formName) + `({${params.join(",")}});`);
    }
  }

  /** PERFORM ... ON COMMIT [LEVEL n] | ON ROLLBACK only registers the subroutine, COMMIT WORK or ROLLBACK WORK runs it */
  private static onCommit(node: abaplint.Nodes.StatementNode, traversal: Traversal, call: string): Chunk {
    const event = /\sON\s+(COMMIT|ROLLBACK)/i.exec(node.concatTokens())?.[1]?.toUpperCase();
    if (event === undefined) {
      return new Chunk(call);
    }
    const formName = node.findDirectExpression(abaplint.Expressions.FormName)?.concatTokens().toUpperCase() ?? "";
    const program = node.findExpressionAfterToken("PROGRAM")?.concatTokens().toUpperCase() ?? traversal.getCurrentObject().getName().toUpperCase();
    const level = node.findExpressionAfterToken("LEVEL");
    const levelCode = level ? ", " + traversal.traverse(level).getCode() : "";
    return new Chunk(`abap.statements.performOn("${event}", ${JSON.stringify(program + "-" + formName)}, async () => {${call}}${levelCode});`);
  }

}
