import {Nodes, Expressions} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class TableExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, source: Chunk): Chunk {
    if (source === undefined) {
      throw new Error("TableExpressionTranspiler: Source chunk is undefined");
    }

    const ret = new Chunk();
    const extra: string[] = [];

    if (node.getChildren().length === 3 && node.findDirectExpression(Expressions.Source)) {
      // index based
      extra.push(`index: ${traversal.traverse(node.findDirectExpression(Expressions.Source)).getCode()}`);
    }

    ret.appendString(`abap.operators.tableExpression(${source.getCode()}, {${extra.join(", ")} })`);
//    ret.appendString("TableExpressionTodo");

    return ret;
  }

}