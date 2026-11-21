import * as abaplint from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

// ABAP decides the calculation type of an arithmetic expression from its operands and the
// target, before it calculates. With an i target, a result outside the range of i is
// CX_SY_ARITHMETIC_OVERFLOW, unless the calculation type is f: then it is the f result that
// does not convert to i, CX_SY_CONVERSION_OVERFLOW, and Integer.set() raises that. The runtime
// calculates an operation before it knows the target, so the check is placed here, around the
// result, with abap.operators.integerResult()

/** whether the statement has a single target of type i */
export function isIntegerTarget(node: abaplint.Nodes.StatementNode, traversal: Traversal): boolean {
  if (node.findDirectExpressions(abaplint.Expressions.Target).length !== 1) {
    return false;
  }
  const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
  return traversal.determineType(node, scope) instanceof abaplint.BasicTypes.IntegerType;
}

/** whether the source is calculated, not a data object or a call that gives one: it has an
 * operator, a sign, or is abs( ), whose result has the type of its argument */
export function isCalculated(source: abaplint.Nodes.ExpressionNode): boolean {
  for (const c of source.getChildren()) {
    if (c instanceof abaplint.Nodes.TokenNode) {
      if (c.getFirstToken().getStr() === "-") {
        return true;
      }
    } else if (c.get() instanceof abaplint.Expressions.ArithOperator) {
      return true;
    } else if (c.get() instanceof abaplint.Expressions.Source) {
      if (isCalculated(c)) {
        return true;
      }
    } else if (c.get() instanceof abaplint.Expressions.MethodCallChain && source.getChildren().length === 1) {
      const call = c.getChildren()[0];
      if (call instanceof abaplint.Nodes.ExpressionNode
          && call.get() instanceof abaplint.Expressions.MethodCall
          && call.findDirectExpression(abaplint.Expressions.MethodName)?.concatTokens().toUpperCase() === "ABS") {
        return true;
      }
    }
  }
  return false;
}

/** whether the calculation type is f, known by ** or an operand of type f */
export function isFloatCalculation(node: abaplint.Nodes.StatementNode, source: abaplint.Nodes.ExpressionNode,
                                   traversal: Traversal): boolean {
  if (source.findAllExpressions(abaplint.Expressions.ArithOperator).some(o => o.concatTokens() === "**")) {
    return true;
  }
  const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
  for (const chain of source.findAllExpressions(abaplint.Expressions.FieldChain)) {
    const children = chain.getChildren();
    if (children.length === 1
        && scope?.findVariable(children[0].concatTokens())?.getType() instanceof abaplint.BasicTypes.FloatType) {
      return true;
    }
  }
  return false;
}

/** the result of ADD, SUBTRACT, MULTIPLY, DIVIDE or +=, -=, *=, /=: the target is an operand */
export function compoundResult(node: abaplint.Nodes.StatementNode, source: abaplint.Nodes.ExpressionNode | undefined,
                               operation: Chunk, traversal: Traversal): Chunk {
  if (source === undefined
      || isIntegerTarget(node, traversal) === false
      || isFloatCalculation(node, source, traversal) === true) {
    return operation;
  }
  return new Chunk().appendString("abap.operators.integerResult(").appendChunk(operation).appendString(")");
}
