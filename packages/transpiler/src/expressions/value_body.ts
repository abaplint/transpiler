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
        const transpiled = new FieldAssignmentTranspiler().transpile(child, traversal).getCode();
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
        if (["THEN", "UNTIL", "WHILE", "FROM", "TO", "WHERE", "GROUPS"].some(token => child.findDirectTokenByText(token))) {
          throw new Error("ValueBody FOR todo, " + body.concatTokens());
        }

        const loop = child.findDirectExpression(Expressions.InlineLoopDefinition);
        if (loop === undefined) {
          throw new Error("ValueBody FOR todo, " + body.concatTokens());
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
for await (const unique1 of abap.statements.loop(${source})) {
  ${targetAction}
  VAL`);
        post = ";\n}\nreturn VAL;\n})()";
      } else {
        throw new Error("ValueBodyTranspiler, unknown " + child.get().constructor.name + " \"" + child.concatTokens()) + "\"";
      }
    }

    return ret.appendString(post);
  }

}