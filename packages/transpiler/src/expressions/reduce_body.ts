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
    } else if (["THEN", "UNTIL", "WHILE", "FROM", "TO", "GROUPS"].some(token => forExpressions[0].findDirectTokenByText(token))) {
      throw new Error("ValueBody FOR todo, " + body.concatTokens());
    }
    const loopExpression = forExpression.findDirectExpression(Expressions.InlineLoopDefinition);
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

    ret.appendString(`for await (const ${loopVariable} of abap.statements.loop(${loopSource}${loopWhere})) {\n`);

    for (const nextChild of body.findDirectExpression(Expressions.ReduceNext)?.getChildren() || []) {
      if (nextChild.get() instanceof Expressions.SimpleTarget && nextChild instanceof Nodes.ExpressionNode) {
        ret.appendString(new TargetTranspiler().transpile(nextChild, traversal).getCode() + ".set(");
      } else if (nextChild.get() instanceof Expressions.Source && nextChild instanceof Nodes.ExpressionNode) {
        ret.appendString(traversal.traverse(nextChild).getCode() + ");\n");
      }
    }

    ret.appendString(`}\n`);

    ret.appendString(`return ${returnField};\n`);

    ret.appendString("})())");
    return ret;
  }

}