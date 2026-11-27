import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DatabaseTableTranspiler} from "../expressions";

export class ModifyDatabaseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const dbtab = node.findFirstExpression(abaplint.Expressions.DatabaseTable);
    if (dbtab === undefined) {
      throw new Error("internal error, ModifyDatabaseTranspiler");
    }

    // "MODIFY itab FROM wa" is parsed as a database MODIFY, but when itab is an internal table
    // it is the short form of the internal table MODIFY: in a LOOP it changes the current line
    const name = dbtab.findDirectExpression(abaplint.Expressions.Dynamic) ? undefined : dbtab.getFirstToken();
    const variable = name ? traversal.findCurrentScopeByToken(name)?.findVariable(name.getStr()) : undefined;
    const from = node.findExpressionAfterToken("FROM");
    if (name && variable && variable.getType() instanceof abaplint.BasicTypes.TableType
        && from && node.findExpressionAfterToken("TABLE") === undefined) {
      const target = traversal.prefixAndName(name, variable.getFilename());
      const source = traversal.traverse(from).getCode();
      return new Chunk(`abap.statements.modifyInternal(${target}, {from: ${source}, ...${ModifyDatabaseTranspiler.currentLine(target)}});`);
    }
    const table = new DatabaseTableTranspiler(false).transpile(dbtab, traversal);

    const options: string[] = [];

    const tab = node.findExpressionAfterToken("TABLE");
    if (tab) {
      const ttab = traversal.traverse(tab);
      options.push(`"table": ` + ttab.getCode());
    }

    if (from && from.get() instanceof abaplint.Expressions.SQLSource) {
      const tvalues = traversal.traverseWithTableContext(dbtab.concatTokens(), from);
      options.push(`"values": ` + tvalues.getCode());
    }

    return new Chunk(`await abap.statements.modifyDatabase(${table.getCode()}, {${options.join(", ")}});`);
  }

  /** the index of the current line, when the table is looped at, else nothing */
  public static currentLine(target: string): string {
    return `(${target}.loops?.size === 1 ? {index: new abap.types.Integer().set(${target}.getCurrentLoopIndex() + 1)} : {})`;
  }

}