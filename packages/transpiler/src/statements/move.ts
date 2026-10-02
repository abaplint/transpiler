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

    let source = this.isInt8Calculation(node, sourceExpression, targetExpressions.length, traversal)
      ? new SourceTranspiler(false, true).transpile(sourceExpression!, traversal)
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

  // The calculation type of an arithmetic expression comes from its operands and the target
  // field. An int8 target with operands of type i makes it int8, so i * i is exact up to 2^62.
  // The runtime decides by the operands alone, it does not know the target, so here the
  // operands are converted to int8 before the calculation. Only for operands whose type is
  // known to be i or int8, and not for **, which has calculation type f
  private isInt8Calculation(node: abaplint.Nodes.StatementNode, source: abaplint.Nodes.ExpressionNode | undefined,
                            targets: number, traversal: Traversal): boolean {
    if (source === undefined
        || targets !== 1
        || source.findFirstExpression(abaplint.Expressions.ArithOperator) === undefined) {
      return false;
    }
    const scope = traversal.findCurrentScopeByToken(node.getFirstToken());
    if (!(traversal.determineType(node, scope) instanceof abaplint.BasicTypes.Integer8Type)) {
      return false;
    }
    return this.integerOperands(source, scope);
  }

  private integerOperands(node: abaplint.Nodes.ExpressionNode, scope: abaplint.ISpaghettiScopeNode | undefined): boolean {
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        if (["(", ")", "-", "+"].includes(c.getFirstToken().getStr()) === false) {
          return false;
        }
      } else if (c.get() instanceof abaplint.Expressions.Source) {
        if (this.integerOperands(c as abaplint.Nodes.ExpressionNode, scope) === false) {
          return false;
        }
      } else if (c.get() instanceof abaplint.Expressions.ArithOperator) {
        if (["+", "-", "*", "/", "DIV", "MOD"].includes(c.concatTokens().toUpperCase()) === false) {
          return false;
        }
      } else if (c.get() instanceof abaplint.Expressions.Constant) {
        if (c.findDirectExpression(abaplint.Expressions.Integer) === undefined) {
          return false;
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
      } else {
        return false;
      }
    }
    return true;
  }

}