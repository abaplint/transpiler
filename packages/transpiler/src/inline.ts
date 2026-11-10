import * as abaplint from "@abaplint/core";
import {Traversal} from "./traversal";
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
      if (traversal.inlineDeclared.has(variable)) {
        continue;
      }
      traversal.inlineDeclared.add(variable);

      result += traversal.declare(variable) + "\n";
    }

// todo: declare DATA and FSes according to when they appear in the code
    const inlineFieldSymbols = node.findAllExpressionsRecursive(abaplint.Expressions.InlineFS);
    for (const expression of inlineFieldSymbols) {
      const target = expression.findFirstExpression(abaplint.Expressions.TargetFieldSymbol);
      if (target === undefined) {
        throw new Error("InlineDeclarations: no target fs field found");
      }
      const variable = traversal.findCurrentScopeByToken(expression.getFirstToken())?.findVariable(target.concatTokens());
      if (variable !== undefined && traversal.inlineDeclared.has(variable)) {
        continue;
      } else if (variable !== undefined) {
        traversal.inlineDeclared.add(variable);
      }

      result += new FieldSymbolTranspiler().transpile(target, traversal).getCode();
    }

    return result;
  }

  /** inline declarations in the code of a program outside its procedures (event blocks, and statements before the
   *  first event), in any nesting: they are global data of the program, declared once at its start */
  public static buildProgramDeclarations(root: abaplint.Nodes.StructureNode, traversal: Traversal): string {
    const program = (e: abaplint.Nodes.ExpressionNode) => {
      const type = traversal.findCurrentScopeByToken(e.getFirstToken())?.getIdentifier().stype;
      return type === abaplint.ScopeType.Program || type === abaplint.ScopeType.SelectionEvent;
    };
    let result = "";
    for (const expression of root.findAllExpressionsRecursive(abaplint.Expressions.InlineData).filter(program)) {
      const name = expression.findFirstExpression(abaplint.Expressions.TargetField)?.concatTokens();
      const variable = name === undefined ? undefined : traversal.findCurrentScopeByToken(expression.getFirstToken())?.findVariable(name);
      if (variable === undefined || traversal.inlineDeclared.has(variable)) {
        continue;
      }
      traversal.inlineDeclared.add(variable);
      result += traversal.declare(variable) + "\n";
    }
    for (const expression of root.findAllExpressionsRecursive(abaplint.Expressions.InlineFS).filter(program)) {
      const target = expression.findFirstExpression(abaplint.Expressions.TargetFieldSymbol);
      const scope = traversal.findCurrentScopeByToken(expression.getFirstToken());
      const variable = target === undefined ? undefined : scope?.findVariable(target.concatTokens());
      if (target === undefined || variable === undefined || traversal.inlineDeclared.has(variable)) {
        continue;
      }
      traversal.inlineDeclared.add(variable);
      result += new FieldSymbolTranspiler().transpile(target, traversal).getCode();
    }
    return result;
  }

}
