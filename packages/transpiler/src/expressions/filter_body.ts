import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";
import {UniqueIdentifier} from "../unique_identifier";

export class FilterBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("FilterBodyTranspiler, Expected TypeNameOrInfer");
    } else if (body.findDirectExpression(Expressions.Let)) {
      throw new Error("FilterBodyTranspiler, Let not supported, todo");
    }

    const source = traversal.traverse(body.findDirectExpression(Expressions.Source)).getCode();
    const type = new TypeNameOrInfer().findType(typ, traversal);
    const target = TranspileTypes.toType(type);

    const ret = new Chunk();

    ret.appendString("(await (async () => {\n");

    /*
    const whereNode = body.findDirectExpression(Expressions.ComponentCond);
    if (whereNode) {
      const where = traversal.traverse(whereNode).getCode();
      extra.push("where: async " + where);
    }
    */

    const id = UniqueIdentifier.get();
    const loop = UniqueIdentifier.get();
    ret.appendString(`const ${id} = ${target};\n`);
    ret.appendString(`for await (const ${loop} of abap.statements.loop(${source})) {\n`);
    ret.appendString(`abap.statements.insertInternal({"table": ${id}, "data": ${loop}});\n`);
    ret.appendString(`}\n`);
    ret.appendString(`return ${id};\n`);

    ret.appendString("})())");
    return ret;
  }

}