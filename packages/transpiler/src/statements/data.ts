import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TranspileTypes} from "../types";
import {Traversal} from "../traversal";

export class DataTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const token = node.findFirstExpression(abaplint.Expressions.DefinitionName)!.getFirstToken();

    const scope = traversal.getSpaghetti().lookupPosition(token.getStart(), traversal.getFilename());
    if (scope === undefined) {
      throw new Error("DataTranspiler, scope not found");
    }

    const found = scope.findVariable(token.getStr());
    if (found === undefined) {
      throw new Error("DataTranspiler, var not found, \"" + token.getStr() + "\"");
    }

// todo, refactor this part to use value from TypedIdentifier
    let value = "";
    const val = node.findFirstExpression(abaplint.Expressions.Value);
    if (val) {
      let int = val.findFirstExpression(abaplint.Expressions.Integer);
      if (int === undefined) {
        int = val.findFirstExpression(abaplint.Expressions.ConstantString);
      }
      if (int){
        value = "\n" + found.getName() + ".set(" + int.getFirstToken().getStr() + ");";
      }
    }

    return new TranspileTypes().declare(found) + value;
  }

}