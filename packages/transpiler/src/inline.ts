import * as abaplint from "@abaplint/core";
import {Traversal} from "./traversal";
import {FieldSymbolTranspiler} from "./statements";

export class InlineDeclarations {

  public static buildDeclarations(node: abaplint.Nodes.StructureNode | abaplint.Nodes.StatementNode,
                                  traversal: Traversal,
                                  programLevelOnly = false): string {
    const include = (expression: abaplint.Nodes.ExpressionNode): boolean => {
      return programLevelOnly === false
        || InlineDeclarations.isProgramLevelScope(traversal.findCurrentScopeByToken(expression.getFirstToken()));
    };

    const inlineDataExpressions = node.findAllExpressionsRecursive(abaplint.Expressions.InlineData).filter(include);
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

      result += traversal.declare(variable) + "\n";
    }

// todo: declare DATA and FSes according to when they appear in the code
    const inlineFieldSymbols = node.findAllExpressionsRecursive(abaplint.Expressions.InlineFS).filter(include);
    for (const expression of inlineFieldSymbols) {
      const target = expression.findFirstExpression(abaplint.Expressions.TargetFieldSymbol);
      if (target === undefined) {
        throw new Error("InlineDeclarations: no target fs field found");
      }

      result += new FieldSymbolTranspiler().transpile(target, traversal).getCode();
    }

    return result;
  }

  /** inline declarations outside FORMs, methods and function modules belong to the program, or to an
   * event block like START-OF-SELECTION, which is also program global */
  public static isProgramLevelScope(scope: abaplint.ISpaghettiScopeNode | undefined): boolean {
    const stype = scope?.getIdentifier().stype;
    return stype === abaplint.ScopeType.Program || stype === abaplint.ScopeType.SelectionEvent;
  }

}
