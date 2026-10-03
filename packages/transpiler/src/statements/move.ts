import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SourceTranspiler} from "../expressions/source";

export class MoveTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const sourceExpression = node.findDirectExpression(abaplint.Expressions.Source);

    const targets: Chunk[] = [];
    const targetExpressions = node.findDirectExpressions(abaplint.Expressions.Target);
    for (const t of targetExpressions) {
      targets.push(traversal.traverse(t));
    }

    const ret = new Chunk();

    if (targetExpressions.length === 1
        && sourceExpression?.getChildren().length === 1
        && sourceExpression.findDirectExpression(abaplint.Expressions.StringTemplate)
        && sourceExpression.concatTokens().toUpperCase().endsWith(" ALPHA = IN }|")
        && sourceExpression.concatTokens().toUpperCase().startsWith("|{")) {
      const target = targets[0].getCode();
      const tSource = traversal.traverse(sourceExpression.findFirstExpression(abaplint.Expressions.StringTemplateSource
        )?.findDirectExpression(abaplint.Expressions.Source));
      ret.appendString(target + `.set(abap.alphaIn(${tSource.getCode()}, ${target}, ${target}));`);
      return ret;
    }

    const packedTarget = this.isPackedTarget(node, sourceExpression, targetExpressions.length, traversal);
    let source = this.isInt8Calculation(node, sourceExpression, targetExpressions.length, traversal)
      ? new SourceTranspiler(false, true).transpile(sourceExpression!, traversal)
      : packedTarget
      ? new SourceTranspiler(false, false, true).transpile(sourceExpression!, traversal)
      : traversal.traverse(sourceExpression);

    const second = node.getChildren()[1]?.concatTokens();
    switch (second) {
      case "?=":
        ret.appendString("await abap.statements.cast(")
          .appendChunk(targets[0])
          .appendString(", ")
          .appendChunk(source)
          .append(");", node.getLastToken(), traversal);
        break;
      case "+":
        ret.appendChunk(targets[0])
          .appendString(".set(abap.operators.add(")
          .appendChunk(targets[0])
          .appendString(", ")
          .appendChunk(source)
          .append("));", node.getLastToken(), traversal);
        break;
      case "-":
        ret.appendChunk(targets[0])
          .appendString(".set(abap.operators.minus(")
          .appendChunk(targets[0])
          .appendString(", ")
          .appendChunk(source)
          .append("));", node.getLastToken(), traversal);
        break;
      case "/=":
        ret.appendChunk(targets[0])
          .appendString(".set(abap.operators.divide(")
          .appendChunk(targets[0])
          .appendString(", ")
          .appendChunk(source)
          .append("));", node.getLastToken(), traversal);
        break;
      case "*=":
        ret.appendChunk(targets[0])
          .appendString(".set(abap.operators.multiply(")
          .appendChunk(targets[0])
          .appendString(", ")
          .appendChunk(source)
          .append("));", node.getLastToken(), traversal);
        break;
      case "&&=":
        ret.appendChunk(targets[0])
          .appendString(".set(abap.operators.concat(")
          .appendChunk(targets[0])
          .appendString(", ")
          .appendChunk(source)
          .append("));", node.getLastToken(), traversal);
        break;
      default:
        for (const target of targets.reverse()) {
          ret.appendChunk(target)
            .appendString(".set(")
            .appendChunk(source)
            .append(");", node.getLastToken(), traversal);
          source = target;
        }
        break;
    }

    return ret;
  }

  // The target and every operand establish one calculation type for the whole RHS.
  // A floating operand anywhere must keep even earlier subexpressions off the
  // packed path: their fourteen-decimal division would already lose precision.
  private isPackedTarget(node: abaplint.Nodes.StatementNode, source: abaplint.Nodes.ExpressionNode | undefined,
                         targets: number, traversal: Traversal): boolean {
    if (source === undefined || targets !== 1
        || source.findFirstExpression(abaplint.Expressions.ArithOperator) === undefined) {
      return false;
    }
    if (source.findAllExpressions(abaplint.Expressions.ArithOperator).some(op => op.concatTokens().trim() === "**")) {
      return false;
    }
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    return traversal.determineType(node, scope) instanceof abaplint.BasicTypes.PackedType
      && this.packedOperands(source, traversal);
  }

  private packedOperands(node: abaplint.Nodes.ExpressionNode, traversal: Traversal): boolean {
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        if (!["(", ")", "-", "+"].includes(c.getFirstToken().getStr())) {
          return false;
        }
      } else if (c.get() instanceof abaplint.Expressions.Source) {
        if (!this.packedOperands(c as abaplint.Nodes.ExpressionNode, traversal)) {
          return false;
        }
      } else if (c.get() instanceof abaplint.Expressions.ArithOperator
          || c.get() instanceof abaplint.Expressions.Constant) {
        continue;
      } else if (c.get() instanceof abaplint.Expressions.FieldChain) {
        const scope = traversal.findCurrentScopeByToken(c.getFirstToken());
        const reference = traversal.findReadOrWriteReference(c.getFirstToken());
        let type = (reference instanceof abaplint.TypedIdentifier ? reference.getType() : undefined)
          ?? scope?.findVariable(c.getFirstToken().getStr())?.getType();
        for (const part of c.getChildren().slice(1)) {
          if (part.get() instanceof abaplint.Expressions.ComponentName) {
            type = Traversal.narrowContextComponent(type, part.concatTokens());
          } else if (part.get() instanceof abaplint.Expressions.AttributeName) {
            type = traversal.narrowContextAttribute(type, part.concatTokens(), scope);
          } else if (part.get() instanceof abaplint.Expressions.TableExpression
              && type instanceof abaplint.BasicTypes.TableType) {
            type = type.getRowType();
          } else if (part.get() instanceof abaplint.Expressions.Dereference
              && type instanceof abaplint.BasicTypes.DataReference) {
            type = type.getType();
          }
        }
        if (!(type instanceof abaplint.BasicTypes.PackedType)
            && !(type instanceof abaplint.BasicTypes.IntegerType)
            && !(type instanceof abaplint.BasicTypes.Integer8Type)
            && !(type instanceof abaplint.BasicTypes.NumericType)
            && !(type instanceof abaplint.BasicTypes.CharacterType)
            && !(type instanceof abaplint.BasicTypes.StringType)) {
          // Includes f, decfloat and generic operands whose runtime type is unknown.
          return false;
        }
      } else {
        // Constructor/function expressions retain ordinary expression dispatch.
        return false;
      }
    }
    return true;
  }

  // The calculation type of an arithmetic expression comes from its operands and the target
  // field, the type with the largest range wins. With operands of type i or int8 it is int8
  // when the target or an operand is int8: i * i into int8 is exact up to 2^62, and "/"
  // rounds. The runtime decides by the operands alone and does not know the target, so here
  // the operands are converted to int8 before the calculation. Only for an i or int8 target
  // and operands whose type is known to be i or int8, and not for **, which counts as f
  private isInt8Calculation(node: abaplint.Nodes.StatementNode, source: abaplint.Nodes.ExpressionNode | undefined,
                            targets: number, traversal: Traversal): boolean {
    if (source === undefined
        || targets !== 1
        || source.findFirstExpression(abaplint.Expressions.ArithOperator) === undefined) {
      return false;
    }
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const target = traversal.determineType(node, scope);
    const found = {int8: target instanceof abaplint.BasicTypes.Integer8Type};
    if (found.int8 === false && !(target instanceof abaplint.BasicTypes.IntegerType)) {
      return false;
    }
    return this.integerOperands(source, scope, found) && found.int8;
  }

  private integerOperands(node: abaplint.Nodes.ExpressionNode, scope: abaplint.ISpaghettiScopeNode | undefined,
                          found: {int8: boolean}): boolean {
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        if (["(", ")", "-", "+"].includes(c.getFirstToken().getStr()) === false) {
          return false;
        }
      } else if (c.get() instanceof abaplint.Expressions.Source) {
        if (this.integerOperands(c as abaplint.Nodes.ExpressionNode, scope, found) === false) {
          return false;
        }
      } else if (c.get() instanceof abaplint.Expressions.ArithOperator) {
        if (["+", "-", "*", "/", "DIV", "MOD"].includes(c.concatTokens().toUpperCase()) === false) {
          return false;
        }
      } else if (c.get() instanceof abaplint.Expressions.Constant) {
        const integer = c.findDirectExpression(abaplint.Expressions.Integer);
        if (integer === undefined) {
          return false;
        } else if (Math.abs(parseInt(integer.concatTokens().replace(/ /g, ""), 10)) > 2147483647) {
          // the transpiler makes an int8 of a literal outside the range of i
          found.int8 = true;
        }
      } else if (c.get() instanceof abaplint.Expressions.FieldChain) {
        const children = c.getChildren();
        if (children.length !== 1) {
          return false;
        }
        const type = scope?.findVariable(children[0].concatTokens())?.getType();
        if (!(type instanceof abaplint.BasicTypes.IntegerType) && !(type instanceof abaplint.BasicTypes.Integer8Type)) {
          return false;
        }
        if (type instanceof abaplint.BasicTypes.Integer8Type) {
          found.int8 = true;
        }
      } else {
        return false;
      }
    }
    return true;
  }

}
