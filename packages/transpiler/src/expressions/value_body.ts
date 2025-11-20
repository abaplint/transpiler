import {Expressions, Nodes, BasicTypes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {ValueBodyLineTranspiler} from "./value_body_line";
import {FieldAssignmentTranspiler} from "./field_assignment";
import {FieldSymbolTranspiler} from "../statements";
import {SourceFieldSymbolTranspiler} from "./source_field_symbol";
import {TranspileTypes} from "../transpile_types";
import {LetTranspiler} from "./let";

export class ValueBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("ValueBodyTranspiler, Expected TypeNameOrInfer");
    }

    let ret = new Chunk().appendString(new TypeNameOrInfer().transpile(typ, traversal).getCode());
    const context = new TypeNameOrInfer().findType(typ, traversal);
    if (context instanceof BasicTypes.VoidType || context instanceof BasicTypes.UnknownType) {
      // compile option is runtime error, or it failed during the validation step
      return new Chunk(TranspileTypes.toType(context));
    }

    let post = "";
    let extraFields = "";
    const hasLines = body.findDirectExpression(Expressions.ValueBodyLine) !== undefined;

    for (const child of body.getChildren()) {
      if (child.get() instanceof Expressions.FieldAssignment && child instanceof Nodes.ExpressionNode) {
        const transpiled = new FieldAssignmentTranspiler().transpile(child, traversal, context).getCode();
        if (hasLines === false) {
          ret.appendString(transpiled);
        } else {
          extraFields += transpiled;
        }
      } else if (child.get() instanceof Expressions.ValueBase && child instanceof Nodes.ExpressionNode) {
        const source = traversal.traverse(child.findDirectExpression(Expressions.Source));
        ret = new Chunk().appendString(source.getCode() + ".clone()");
      } else if (child.get() instanceof Expressions.ValueBodyLine && child instanceof Nodes.ExpressionNode) {
        if (!(context instanceof BasicTypes.TableType)) {
          throw new Error("ValueBodyTranspiler, Expected BasicTypes, " + body.concatTokens());
        }
        const rowType = context.getRowType();
        ret.appendString(new ValueBodyLineTranspiler().transpile(rowType, child, traversal, extraFields).getCode());
      } else if (child.get() instanceof Expressions.Source && child instanceof Nodes.ExpressionNode) {
        const source = traversal.traverse(child);
        ret.appendString(".set(" + source.getCode() + ".clone())");
      } else if (child.get() instanceof Expressions.For && child instanceof Nodes.ExpressionNode) {
        const loop = child.findDirectExpression(Expressions.InlineLoopDefinition);
        if (loop) {
          if (["THEN", "UNTIL", "WHILE", "FROM", "TO", "GROUPS"].some(token => child.findDirectTokenByText(token))) {
            throw new Error("ValueBody FOR todo, " + body.concatTokens());
          }

          let loopWhere = "";
          const whereNode = child?.findDirectExpression(Expressions.ComponentCond);
          if (whereNode) {
            const where = traversal.traverse(whereNode).getCode();
            loopWhere = `, {"where": async ` + where + `}`;
          }

          const base = loop.findDirectExpression(Expressions.ValueBase);
          if (base) {
            throw new Error("ValueBody FOR todo, base, " + body.concatTokens());
          }

          let targetDeclare = "";
          let targetAction = "";
          const fs = loop.findDirectExpression(Expressions.TargetFieldSymbol);
          if (fs) {
            targetDeclare = new FieldSymbolTranspiler().transpile(fs, traversal).getCode();
            const targetName = new SourceFieldSymbolTranspiler().transpile(fs, traversal).getCode();
            targetAction = `${targetName}.assign(unique1);`;
          } else {
            const field = traversal.traverse(loop.findDirectExpression(Expressions.TargetField));
            if (field === undefined) {
              throw new Error("ValueBody FOR empty field todo, " + body.concatTokens());
            }
            targetAction = `const ${field.getCode()} = unique1.clone();`;
          }

          const llet = child.findDirectExpression(Expressions.Let);
          if (llet) {
            targetAction += new LetTranspiler().transpile(llet, traversal).getCode();
          }

          const source = traversal.traverse(loop.findDirectExpression(Expressions.Source)).getCode();
          const val = new TypeNameOrInfer().transpile(typ, traversal).getCode();

          ret = new Chunk().appendString(`await (async () => {
${targetDeclare}
const VAL = ${val};
for await (const unique1 of abap.statements.loop(${source}${loopWhere})) {
  ${targetAction}
  VAL`);
          post = ";\n}\nreturn VAL;\n})()";
        } else {
          const counter = child.findDirectExpression(Expressions.InlineFieldDefinition);
          if (counter === undefined) {
            throw new Error("ValueBody FOR todo, " + body.concatTokens());
          }

          if (["GROUPS", "FROM", "TO"].some(token => child.findDirectTokenByText(token))) {
            throw new Error("ValueBody FOR todo, " + body.concatTokens());
          }

          if (child.findDirectExpression(Expressions.ComponentCond)) {
            throw new Error("ValueBody FOR todo, component cond, " + body.concatTokens());
          }

          const cond = child.findDirectExpression(Expressions.Cond);
          if (cond === undefined) {
            throw new Error("ValueBody FOR missing condition, " + body.concatTokens());
          }

          const hasUntil = child.findDirectTokenByText("UNTIL") !== undefined;
          const hasWhile = child.findDirectTokenByText("WHILE") !== undefined;
          if ((hasUntil ? 1 : 0) + (hasWhile ? 1 : 0) !== 1) {
            throw new Error("ValueBody FOR todo, condition, " + body.concatTokens());
          }

          const fieldExpr = counter.findDirectExpression(Expressions.Field);
          const fieldName = fieldExpr?.concatTokens().toLowerCase();
          if (fieldName === undefined) {
            throw new Error("ValueBody FOR todo, inline field, " + body.concatTokens());
          }
          const scope = traversal.findCurrentScopeByToken(counter.getFirstToken());
          const variable = scope?.findVariable(fieldName);
          if (variable === undefined) {
            throw new Error("ValueBody FOR todo, variable, " + body.concatTokens());
          }
          const declare = TranspileTypes.declare(variable);
          const counterName = Traversal.prefixVariable(fieldName);

          const startSource = counter.findDirectExpression(Expressions.Source);
          if (startSource === undefined) {
            throw new Error("ValueBody FOR missing initial value, " + body.concatTokens());
          }
          const start = traversal.traverse(startSource).getCode();

          const thenExpr = child.findExpressionAfterToken("THEN");
          let incrementExpression = "";
          if (thenExpr && thenExpr instanceof Nodes.ExpressionNode) {
            incrementExpression = traversal.traverse(thenExpr).getCode();
          } else {
            incrementExpression = `abap.operators.add(${counterName}, new abap.types.Integer().set(1))`;
          }
          const incrementLine = `${counterName}.set(${incrementExpression});`;

          const llet = child.findDirectExpression(Expressions.Let);
          let letCode = "";
          if (llet) {
            letCode = new LetTranspiler().transpile(llet, traversal).getCode();
          }
          const letSection = letCode === "" ? "" : "  " + letCode.replace(/\n/g, "\n  ") + "\n";

          const condCode = traversal.traverse(cond).getCode();
          const val = new TypeNameOrInfer().transpile(typ, traversal).getCode();

          const preCheck = hasWhile ? `  if (!(${condCode})) {\n    break;\n  }\n` : "";
          let postLoop = `  ${incrementLine}\n`;
          if (hasUntil) {
            postLoop += `  if (${condCode}) {\n    break;\n  }\n`;
          }

          ret = new Chunk().appendString(`await (async () => {
${declare}
${counterName}.set(${start});
const VAL = ${val};
while (true) {
${preCheck}${letSection}  VAL`);
          post = ";\n" + postLoop + "}\nreturn VAL;\n})()";
        }
      } else if (child instanceof Nodes.TokenNode && child.getFirstToken().getStr().toUpperCase() === "DEFAULT") {
        // note: this is last in the body, so its okay to prepend and postpend
        const sources = body.findDirectExpressions(Expressions.Source);
        const deflt = traversal.traverse(sources[1]).getCode();
        const pre = `(await (async () => { try { return `;
        ret = new Chunk().appendString(pre + ret.getCode());
        post += `; } catch (error) { if (abap.isLineNotFound(error)) { return ${deflt}; } throw error; } })())`;
      } else if (child instanceof Nodes.TokenNode && child.getFirstToken().getStr().toUpperCase() === "OPTIONAL") {
        // note: this is last in the body, so its okay to prepend and postpend
        const pre = `(await (async () => { try { return `;
        ret = new Chunk().appendString(pre + ret.getCode());
        post += `; } catch (error) { if (abap.isLineNotFound(error)) { return ${TranspileTypes.toType(context)}; } throw error; } })())`;
      } else {
        throw new Error("ValueBodyTranspiler, unknown " + child.get().constructor.name + " \"" + child.concatTokens()) + "\"";
      }
    }

    return ret.appendString(post);
  }

}