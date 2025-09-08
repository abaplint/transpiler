import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {TranspileTypes} from "../transpile_types";
import {UniqueIdentifier} from "../unique_identifier";

export class ReduceBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("ReduceBodyTranspiler, Expected TypeNameOrInfer");
    } else if (body.findDirectTokenByText("EXCEPT")) {
      return new Chunk(`(() => { throw new Error("ReduceBodyTranspiler EXCEPT in, not supported, transpiler"); })()`);
    }

    const source = traversal.traverse(body.findDirectExpression(Expressions.Source)).getCode();
    const type = new TypeNameOrInfer().findType(typ, traversal);
    const target = TranspileTypes.toType(type);

    const ret = new Chunk();

    ret.appendString("(await (async () => {\n");

    let loopWhere = "";
    const whereNode = body.findDirectExpression(Expressions.ComponentCond);
    if (whereNode) {
      const where = traversal.traverse(whereNode).getCode();
      loopWhere = `, {"where": async ` + where + `}`;
    }

    const id = UniqueIdentifier.get();
    const loop = UniqueIdentifier.get();
    ret.appendString(`const ${id} = ${target};\n`);
    ret.appendString(`for await (const ${loop} of abap.statements.loop(${source}${loopWhere})) {\n`);
    ret.appendString(`abap.statements.insertInternal({"table": ${id}, "data": ${loop}});\n`);
    ret.appendString(`}\n`);
    ret.appendString(`return ${id};\n`);

    ret.appendString("})())");
    return ret;
  }

}