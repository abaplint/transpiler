import * as abaplint from "@abaplint/core";
import {Traversal} from "./traversal";
import {TranspileTypes} from "./transpile_types";

export class InlineDeclarations {

  public static buildDeclarations(node: abaplint.Nodes.StructureNode, traversal: Traversal): string {
    const expressions = node.findAllExpressionsRecursive(abaplint.Expressions.InlineData);
    let result = "";

    for (const expression of expressions) {
      const name = expression.findFirstExpression(abaplint.Expressions.TargetField)?.concatTokens();
      if (name === undefined) {
        throw new Error("InlineDeclarations: no target field found");
      }
      const scope = traversal.findCurrentScopeByToken(expression.getFirstToken());
      const variable = scope?.findVariable(name);
      if (variable === undefined) {
        throw new Error("InlineDeclarations: no variable found");
      }

      result += TranspileTypes.declare(variable) + "\n";
    }
    return result;
  }

}