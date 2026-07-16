import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {TargetTranspiler} from "./target";

export class ReduceBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("ReduceBodyTranspiler, Expected TypeNameOrInfer");
    } else if (body.findDirectExpression(Expressions.Let) !== undefined) {
      return new Chunk(`(() => { throw new Error("ReduceBodyTranspiler LET, not supported, transpiler"); })()`);
    }

    const forExpressions = body.findDirectExpressions(Expressions.For);
    const forExpression = forExpressions[0];
    if (forExpressions.length === 0) {
      throw new Error("ReduceBodyTranspiler, expected FOR");
    } else if (forExpressions.length > 1) {
      throw new Error("ReduceBodyTranspiler, multiple FOR not supported, " + body.concatTokens());
    }

    const loopExpression = forExpression.findDirectExpression(Expressions.InlineLoopDefinition);

    if (loopExpression === undefined) {
      // index based FOR, eg. "FOR i = 1 WHILE i <= 5"
      return this.transpileIndex(body, forExpression, traversal);
    } else if (["THEN", "UNTIL", "WHILE", "FROM", "TO", "GROUPS"].some(token => forExpression.findDirectTokenByText(token))) {
      throw new Error("ValueBody FOR todo, " + body.concatTokens());
    }

    const loopSource = traversal.traverse(loopExpression?.findDirectExpression(Expressions.Source)).getCode();

    const loopVariable = traversal.traverse(loopExpression?.findDirectExpression(Expressions.TargetField)
      || loopExpression?.findDirectExpression(Expressions.TargetFieldSymbol)).getCode();

//    const type = new TypeNameOrInfer().findType(typ, traversal);
//    const target = TranspileTypes.toType(type);

    const ret = new Chunk();

    ret.appendString("(await (async () => {\n");

    let loopWhere = "";
    const whereNode = forExpression?.findDirectExpression(Expressions.ComponentCond);
    if (whereNode) {
      const where = traversal.traverse(whereNode).getCode();
      loopWhere = `, {"where": async ` + where + `}`;
    }

    /*
    const returnId = UniqueIdentifier.get();
    ret.appendString(`const ${returnId} = ${target};\n`);
    */

    const returnField = this.declareInit(body, traversal, ret);

    ret.appendString(`for await (const ${loopVariable} of abap.statements.loop(${loopSource}${loopWhere})) {\n`);

    ret.appendString(this.transpileNext(body, traversal));

    ret.appendString(`}\n`);

    ret.appendString(`return ${returnField};\n`);

    ret.appendString("})())");
    return ret;
  }

  private transpileIndex(body: Nodes.ExpressionNode, forExpression: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    if (["FROM", "TO", "GROUPS"].some(token => forExpression.findDirectTokenByText(token))) {
      throw new Error("ValueBody FOR todo, " + body.concatTokens());
    }

    const counter = forExpression.findDirectExpression(Expressions.InlineFieldDefinition);
    if (counter === undefined) {
      throw new Error("ValueBody FOR todo, " + body.concatTokens());
    }

    const cond = forExpression.findDirectExpression(Expressions.Cond);
    if (cond === undefined) {
      throw new Error("ValueBody FOR missing condition, " + body.concatTokens());
    }

    const hasUntil = forExpression.findDirectTokenByText("UNTIL") !== undefined;
    const hasWhile = forExpression.findDirectTokenByText("WHILE") !== undefined;
    if ((hasUntil ? 1 : 0) + (hasWhile ? 1 : 0) !== 1) {
      throw new Error("ValueBody FOR todo, condition, " + body.concatTokens());
    }

    const fieldName = counter.findDirectExpression(Expressions.Field)?.concatTokens().toLowerCase();
    if (fieldName === undefined) {
      throw new Error("ValueBody FOR todo, inline field, " + body.concatTokens());
    }
    const scope = traversal.findCurrentScopeByToken(counter.getFirstToken());
    const variable = scope?.findVariable(fieldName);
    if (variable === undefined) {
      throw new Error("ValueBody FOR todo, variable, " + body.concatTokens());
    }
    const counterName = Traversal.prefixVariable(fieldName);

    const startSource = counter.findDirectExpression(Expressions.Source);
    if (startSource === undefined) {
      throw new Error("ValueBody FOR missing initial value, " + body.concatTokens());
    }
    const start = traversal.traverse(startSource).getCode();

    const thenExpr = forExpression.findExpressionAfterToken("THEN");
    let incrementExpression = "";
    if (thenExpr && thenExpr instanceof Nodes.ExpressionNode) {
      incrementExpression = traversal.traverse(thenExpr).getCode();
    } else {
      incrementExpression = `abap.operators.add(${counterName}, new abap.types.Integer().set(1))`;
    }
    const condCode = traversal.traverse(cond).getCode();

    const ret = new Chunk();
    ret.appendString("(await (async () => {\n");

    const returnField = this.declareInit(body, traversal, ret);

    ret.appendString(TranspileTypes.declare(variable) + `\n`);
    ret.appendString(`${counterName}.set(${start});\n`);
    ret.appendString(`while (true) {\n`);
    if (hasWhile) {
      ret.appendString(`if (!(${condCode})) {\nbreak;\n}\n`);
    }

    ret.appendString(this.transpileNext(body, traversal));

    ret.appendString(`${counterName}.set(${incrementExpression});\n`);
    if (hasUntil) {
      ret.appendString(`if (${condCode}) {\nbreak;\n}\n`);
    }
    ret.appendString(`}\n`);

    ret.appendString(`return ${returnField};\n`);
    ret.appendString("})())");
    return ret;
  }

  private declareInit(body: Nodes.ExpressionNode, traversal: Traversal, ret: Chunk): string {
    let returnField = "";
    for (const init of body.findDirectExpressions(Expressions.InlineFieldDefinition)) {
      const fieldName = init.findDirectExpression(Expressions.Field)!.concatTokens().toLowerCase();
      returnField = fieldName;
      const scope = traversal.findCurrentScopeByToken(init.getFirstToken());
      const variable = scope?.findVariable(fieldName);
      if (variable === undefined) {
        throw new Error(`ReduceBodyTranspiler: variable ${fieldName} not found`);
      }
      ret.appendString(TranspileTypes.declare(variable) + `\n`);
    }
    return returnField;
  }

  private transpileNext(body: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";
    for (const nextChild of body.findDirectExpression(Expressions.ReduceNext)?.getChildren() || []) {
      if (nextChild.get() instanceof Expressions.SimpleTarget && nextChild instanceof Nodes.ExpressionNode) {
        ret += new TargetTranspiler().transpile(nextChild, traversal).getCode() + ".set(";
      } else if (nextChild.get() instanceof Expressions.Source && nextChild instanceof Nodes.ExpressionNode) {
        ret += traversal.traverse(nextChild).getCode() + ");\n";
      }
    }
    return ret;
  }

}