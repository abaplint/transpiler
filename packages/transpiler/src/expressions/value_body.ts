import {Expressions, Nodes, BasicTypes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {ValueBodyLineTranspiler} from "./value_body_line";
import {FieldAssignmentTranspiler} from "./field_assignment";
import { FieldSymbolTranspiler } from "../statements";

export class ValueBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("ValueBodyTranspiler, Expected TypeNameOrInfer");
    }

    let ret = new Chunk().appendString(new TypeNameOrInfer().transpile(typ, traversal).getCode());
    const context = new TypeNameOrInfer().findType(typ, traversal);
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
          throw new Error("ValueBodyTranspiler, Expected BasicTypes");
        }
        const rowType = context.getRowType();
        ret.appendString(new ValueBodyLineTranspiler().transpile(rowType, child, traversal, extraFields).getCode());
      } else if (child.get() instanceof Expressions.Source && child instanceof Nodes.ExpressionNode) {
        const source = traversal.traverse(child);
        ret.appendString(".set(" + source.getCode() + ".clone())");
      } else if (child.get() instanceof Expressions.For && child instanceof Nodes.ExpressionNode) {
        if (child.getChildren().length !== 2) {
          throw new Error("ValueBody FOR todo, num");
        }
        const loop = child.findDirectExpression(Expressions.InlineLoopDefinition);
        if (loop === undefined) {
          throw new Error("ValueBody FOR todo");
        } else if (loop.getChildren().length !== 3) {
          throw new Error("ValueBody FOR todo, num loop");
        }
        const fs = loop.findDirectExpression(Expressions.TargetFieldSymbol);
        if (fs === undefined) {
          throw new Error("ValueBody FOR target");
        }
        const target = new FieldSymbolTranspiler().transpile(fs, traversal);

        const val = new TypeNameOrInfer().transpile(typ, traversal).getCode();

        ret = new Chunk().appendString(`await (async () => {
${target.getCode()}
const VAL = ${val};
for await (const unique1 of abap.statements.loop(input)) {
  fs_input_.assign(unique1);
  VAL`);
        post = ";\n}\nreturn VAL;\n})()";
      } else {
        throw new Error("ValueBodyTranspiler, unknown " + child.get().constructor.name + " \"" + child.concatTokens()) + "\"";
      }
    }

    return ret.appendString(post);
  }

}