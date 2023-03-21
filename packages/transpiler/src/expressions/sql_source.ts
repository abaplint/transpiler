import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SQLSourceTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let s = node.findDirectExpression(abaplint.Expressions.Source);
    if (s?.getChildren().length === 1 && s.getChildren()[0].get() instanceof abaplint.Expressions.SQLAliasField) {
      const chunk = new Chunk();
      let concat = s.concatTokens();
      if (concat.includes("~") && concat.split("~")[0].includes("/")) {
        concat = "'" + concat.replace("~", "'~");
      }
      chunk.appendString(concat);
      return chunk;
    }

    if (s === undefined) {
      s = node.findDirectExpression(abaplint.Expressions.SimpleSource3);
    }
    return traversal.traverse(s);
  }

}