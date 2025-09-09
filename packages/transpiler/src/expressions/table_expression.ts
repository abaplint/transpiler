import {Nodes, Expressions} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {ComponentChainSimpleTranspiler} from "./component_chain_simple";

export class TableExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, source: Chunk): Chunk {
    if (source === undefined) {
      throw new Error("TableExpressionTranspiler: Source chunk is undefined");
    }

    const ret = new Chunk();
    const extra: string[] = [];
    let field = "";
    let usesTableLine = false;
    const withKey: string[] = [];

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
          field = new ComponentChainSimpleTranspiler("i.").transpile(c, traversal).getCode();
          if (field === "i.table_line") {
            usesTableLine = true;
          }
        } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.Source) {
          withKey.push("abap.compare.eq(" + field + ", " + traversal.traverse(c).getCode() + ")");
        } else if (c.concatTokens() === "=" || c.concatTokens() === "[" || c.concatTokens() === "]") {
          continue;
        } else {
          throw new Error("TableExpressionTranspiler: todo, other, " + c.concatTokens());
        }
      }

      extra.push("withKey: async (i) => {return " + withKey.join(" && ") + ";}");
      extra.push("usesTableLine: " + usesTableLine);
    }

    ret.appendString(`(await abap.operators.tableExpression(${source.getCode()}, {${extra.join(", ")} }))`);

    return ret;
  }

}