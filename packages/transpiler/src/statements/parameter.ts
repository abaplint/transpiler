import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {SelectionDefault} from "./_selection";

export class ParameterTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const variable = SelectionDefault.variable(node, traversal);
    // abaplint types "TYPE c LENGTH 3" and "TYPE p ... DECIMALS 2" of a parameter
    // without the length and the decimals, a value would be cut silently
    const tokens = node.getTokens().map(t => t.getStr().toUpperCase());
    const sized = tokens.some((t, i) => (t === "LENGTH" && tokens[i - 1] !== "VISIBLE") || t === "DECIMALS");
    if (variable === undefined || sized === true) {
      return new Chunk(`throw new Error("Parameter, not supported, transpiler");`);
    }
    const {name, type} = variable;

    const ret = new Chunk().appendString("let " + name + " = " + TranspileTypes.toType(type) + ";");

    const operand = SelectionDefault.after(node, "DEFAULT");
    if (operand) {
      ret.appendString(SelectionDefault.set(node, name, type, operand, traversal));
    } else if (this.isFirstOfUnsetRadioGroup(node, traversal)) {
      // a radio button group without DEFAULT 'X' starts with its first button chosen
      ret.appendString("\n" + name + ".set('X');");
    }

    return ret;
  }

  private isFirstOfUnsetRadioGroup(node: abaplint.Nodes.StatementNode, traversal: Traversal): boolean {
    const group = node.findDirectExpression(abaplint.Expressions.RadioGroupName)?.concatTokens().toUpperCase();
    if (group === undefined) {
      return false;
    }
    const members = traversal.getFile().getStatements().filter(s => s.get() instanceof abaplint.Statements.Parameter
      && s.findDirectExpression(abaplint.Expressions.RadioGroupName)?.concatTokens().toUpperCase() === group);
    if (members.some(m => SelectionDefault.after(m, "DEFAULT") !== undefined)) {
      return false;
    }
    return members[0].getStart().equals(node.getStart());
  }

}
