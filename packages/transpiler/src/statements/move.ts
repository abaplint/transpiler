import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SourceTranspiler} from "../expressions/source";
import {TypeNameOrInfer} from "../expressions/type_name_or_infer";
import {CorrespondingBodyTranspiler} from "../expressions/corresponding_body";

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

    const second = node.getChildren()[1]?.concatTokens();
    let source = second === "=" && targetExpressions.length === 1
      ? this.correspondingGeneric(sourceExpression, targets[0], traversal)
      : undefined;
    if (source === undefined) {
      let calculation = this.integerCalculation(node, sourceExpression, targetExpressions.length, traversal);
      if (calculation === "i" && second !== "=") {
        calculation = undefined;
      }
      source = calculation !== undefined
        ? new SourceTranspiler(false, calculation).transpile(sourceExpression!, traversal)
        : traversal.traverse(sourceExpression);
    }

    switch (second) {
      case "?=":
        ret.appendString("await abap.statements.cast(")
          .appendChunk(targets[0])
          .appendString(", ")
          .appendChunk(source)
          .appendString(", " + traversal.buildExtraCX(node))
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

  // "target = CORRESPONDING #( ... )" with a generic target, eg. "CHANGING ct TYPE STANDARD TABLE",
  // the type of the result is only known at runtime, so the target is passed on
  private correspondingGeneric(source: abaplint.Nodes.ExpressionNode | undefined, target: Chunk,
                               traversal: Traversal): Chunk | undefined {
    const children = source?.getChildren() || [];
    const typ = source?.findDirectExpression(abaplint.Expressions.TypeNameOrInfer);
    const body = source?.findDirectExpression(abaplint.Expressions.CorrespondingBody);
    if (children.length !== 5
        || children[0].concatTokens().toUpperCase() !== "CORRESPONDING"
        || typ === undefined
        || typ.concatTokens() !== "#"
        || body === undefined
        || new TypeNameOrInfer().findTypeOrUndefined(typ, traversal)?.isGeneric() !== true) {
      return undefined;
    }
    return new CorrespondingBodyTranspiler().transpile(typ, body, traversal, target.getCode());
  }

  // The calculation type of an arithmetic expression comes from its operands and the target
  // field, the type with the largest range wins. With operands of type i or int8 it is int8
  // when the target or an operand is int8: i * i into int8 is exact up to 2^62, and "/"
  // rounds. The runtime decides by the operands alone and does not know the target, so here
  // the operands are converted to int8 before the calculation. Only for an i or int8 target
  // and operands whose type is known to be i or int8, and not for **, which counts as f.
  // With a p, f or decfloat34 target the calculation type is that of the target, and an int8
  // result outside the int8 range is no overflow: the operands are converted to decfloat34,
  // exact for sums, differences and products of int8, so the runtime does not raise for them.
  // With an i target and operands of type i only it is i, and then "+", "-" and "*" raise
  // CX_SY_ARITHMETIC_OVERFLOW outside the range of i: SourceTranspiler picks the operators that
  // check it. An expression with "/", DIV or MOD keeps the unchecked operators for now
  private integerCalculation(node: abaplint.Nodes.StatementNode, source: abaplint.Nodes.ExpressionNode | undefined,
                             targets: number, traversal: Traversal): "int8" | "decfloat34" | "i" | undefined {
    if (source === undefined
        || targets !== 1
        || source.findFirstExpression(abaplint.Expressions.ArithOperator) === undefined) {
      return undefined;
    }
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    const target = traversal.determineType(node, scope);
    const wide = target instanceof abaplint.BasicTypes.PackedType
      || target instanceof abaplint.BasicTypes.FloatType
      || target instanceof abaplint.BasicTypes.DecFloat34Type;
    const found = {int8: target instanceof abaplint.BasicTypes.Integer8Type};
    if (found.int8 === false && wide === false && !(target instanceof abaplint.BasicTypes.IntegerType)) {
      return undefined;
    } else if (this.integerOperands(source, scope, found) === false) {
      return undefined;
    } else if (found.int8 === false) {
      const division = source.findAllExpressions(abaplint.Expressions.ArithOperator)
        .some(o => ["/", "DIV", "MOD"].includes(o.concatTokens().toUpperCase()));
      return wide === false && division === false ? "i" : undefined;
    } else if (wide === true) {
      // DIV and MOD of a decfloat34 go through a double in the runtime, they stay int8
      const divMod = source.findAllExpressions(abaplint.Expressions.ArithOperator)
        .some(o => ["DIV", "MOD"].includes(o.concatTokens().toUpperCase()));
      return divMod ? undefined : "decfloat34";
    }
    return "int8";
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