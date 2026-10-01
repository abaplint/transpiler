import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

/** SUBMIT ... AND RETURN with WITH additions, run by the runtime's SubmitHost.
 * Everything else SUBMIT can say (no AND RETURN, VIA JOB, VIA SELECTION-SCREEN,
 * selection tables and sets, list and spool options) throws when reached */
export class SubmitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const children = node.getChildren();
    const refuse = (what: string) => new Chunk(`throw new Error("SUBMIT, ${what} not supported, transpiler");`);

    let program: string;
    const name = children[1];
    if (name instanceof abaplint.Nodes.ExpressionNode && name.get() instanceof abaplint.Expressions.Dynamic) {
      const inner = name.findDirectExpression(abaplint.Expressions.FieldChain)
        ?? name.findDirectExpression(abaplint.Expressions.Constant);
      if (inner === undefined) {
        return refuse("dynamic program name");
      }
      program = traversal.traverse(inner).getCode() + ".get().trimEnd().toUpperCase()";
    } else {
      program = JSON.stringify(name.concatTokens().toUpperCase());
    }

    const selections: string[] = [];
    let andReturn = false;
    let i = 2;
    const word = (at: number) => children[at]?.concatTokens().toUpperCase();
    const source = (at: number) => traversal.traverse(children[at]).getCode();
    while (i < children.length) {
      const current = children[i];
      if (current instanceof abaplint.Nodes.ExpressionNode && current.get() instanceof abaplint.Expressions.AndReturn) {
        andReturn = true;
        i++;
      } else if (word(i) === "." ) {
        i++;
      } else if (word(i) === "WITH" && children[i + 1]?.get() instanceof abaplint.Expressions.FieldSub) {
        const sel = JSON.stringify(word(i + 1));
        const operator = word(i + 2);
        let next: number;
        let low: string;
        let high: string | undefined;
        if (operator === "BETWEEN") {
          low = source(i + 3);
          high = source(i + 5);
          next = i + 6;
        } else if (operator === "IN") {
          selections.push(`{name: ${sel}, table: ${source(i + 3)}}`);
          i += 4;
          continue;
        } else if (["=", "EQ", "NE", "CP", "GE", "LE", "GT", "LT"].includes(operator ?? "")) {
          low = source(i + 3);
          next = i + 4;
        } else {
          return refuse("WITH " + operator);
        }
        let sign: string | undefined;
        if (word(next) === "SIGN") {
          sign = source(next + 1);
          next += 2;
        }
        const option = operator === "BETWEEN" ? "BT" : operator === "=" ? "EQ" : operator!;
        if (option === "EQ" && sign === undefined) {
          // a parameter's value, or for a select-option one I EQ row
          selections.push(`{name: ${sel}, value: ${low}}`);
        } else {
          selections.push(`{name: ${sel}, sign: ${sign ?? "'I'"}, option: '${option}', low: ${low}` +
            (high ? `, high: ${high}` : "") + "}");
        }
        i = next;
      } else {
        return refuse(current.concatTokens());
      }
    }
    if (andReturn === false) {
      return refuse("without AND RETURN");
    }

    return new Chunk(`await abap.statements.submit({program: ${program}, selections: [${selections.join(", ")}]});`);
  }

}
