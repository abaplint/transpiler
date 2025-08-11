import * as abaplint from "@abaplint/core";
import {Traversal} from "./traversal";
import {TranspileTypes} from "./transpile_types";
import {FieldSymbolTranspiler} from "./statements";

export class InlineDeclarations {

  public static buildDeclarations(node: abaplint.Nodes.StructureNode, traversal: Traversal): string {
    const inlineDataExpressions = node.findAllExpressionsRecursive(abaplint.Expressions.InlineData);
    let result = "";

    for (const expression of inlineDataExpressions) {
      const name = expression.findFirstExpression(abaplint.Expressions.TargetField)?.concatTokens();
      if (name === undefined) {
        throw new Error("InlineDeclarations: no target data field found");
      }
      const scope = traversal.findCurrentScopeByToken(expression.getFirstToken());
      const variable = scope?.findVariable(name);
      if (variable === undefined) {
        throw new Error("InlineDeclarations: no variable found");
      }

      result += TranspileTypes.declare(variable) + "\n";
    }

// todo: declare DATA and FSes according to when they appear in the code
    const inlineFieldSymbols = node.findAllExpressionsRecursive(abaplint.Expressions.InlineFS);
    for (const expression of inlineFieldSymbols) {
      const target = expression.findFirstExpression(abaplint.Expressions.TargetFieldSymbol);
      if (target === undefined) {
        throw new Error("InlineDeclarations: no target fs field found");
      }

      result += new FieldSymbolTranspiler().transpile(target, traversal).getCode();
    }

    return result;
  }

}