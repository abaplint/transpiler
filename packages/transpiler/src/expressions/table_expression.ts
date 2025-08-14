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
      extra.push(`index: ${traversal.traverse(node.findDirectExpression(Expressions.Source)).getCode()}`);
    } else if (node.findDirectTokenByText("INDEX")) {
      throw new Error("TableExpressionTranspiler: todo, other INDEX");
    } else if (node.findDirectExpression(Expressions.SimpleName)) {
      throw new Error("TableExpressionTranspiler: todo, SimpleName");
    } else if (node.findDirectExpression(Expressions.Dynamic)) {
      throw new Error("TableExpressionTranspiler: todo, Dynamic");
    } else {
      // conditions
      for (const c of node.getChildren()) {
        if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ComponentChainSimple) {
          // todo
        } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Source) {
          // todo
          traversal.traverse(c);
        } else if (c.concatTokens() === "=" || c.concatTokens() === "[" || c.concatTokens() === "]") {
          continue;
        } else {
          throw new Error("TableExpressionTranspiler: todo, other, " + c.concatTokens());
        }
      }
    }

    ret.appendString(`abap.operators.tableExpression(${source.getCode()}, {${extra.join(", ")} })`);

    return ret;
  }

}