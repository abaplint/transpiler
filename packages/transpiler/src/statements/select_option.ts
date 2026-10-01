import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {SelectionDefault} from "./_selection";

export class SelectOptionTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const variable = SelectionDefault.variable(node, traversal);
    const row = variable?.type instanceof abaplint.BasicTypes.TableType ? variable.type.getRowType() : undefined;
    const lowType = row instanceof abaplint.BasicTypes.StructureType ? row.getComponentByName("LOW") : undefined;
    if (variable === undefined || lowType === undefined) {
      // eg. FOR (dynamic)
      return new Chunk(`throw new Error("SelectOption, not supported, transpiler");`);
    }
    const {name, type} = variable;

    const ret = new Chunk().appendString("let " + name + " = " + TranspileTypes.toType(type) + ";");

    // DEFAULT low [TO high] [OPTION o] [SIGN s] is the first row of the table and
    // stays in the header line: I EQ, or I BT with TO, unless OPTION or SIGN say otherwise
    const low = SelectionDefault.after(node, "DEFAULT");
    if (low === undefined) {
      return ret.appendString(SelectionDefault.submitted(node, traversal, name, "selectionSelectOption"));
    }
    const high = SelectionDefault.after(node, "TO");
    const option = SelectionDefault.after(node, "OPTION")?.concatTokens().toUpperCase() ?? (high ? "BT" : "EQ");
    const sign = SelectionDefault.after(node, "SIGN")?.concatTokens().toUpperCase() ?? "I";

    ret.appendString("\n" + name + ".get().sign.set('" + sign + "');");
    ret.appendString("\n" + name + ".get().option.set('" + option + "');");
    ret.appendString(SelectionDefault.set(node, name + ".get().low", lowType, low, traversal));
    if (high) {
      ret.appendString(SelectionDefault.set(node, name + ".get().high", lowType, high, traversal));
    }
    ret.appendString("\nabap.statements.append({source: " + name + "});");
    return ret.appendString(SelectionDefault.submitted(node, traversal, name, "selectionSelectOption"));
  }

}
