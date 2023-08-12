import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class DatabaseTableTranspiler implements IExpressionTranspiler {
  private readonly prefix: boolean;

  public constructor(prefix = true) {
    this.prefix = prefix;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();
    const concat = node.concatTokens();

    let val = "";
    if (this.prefix === true) {
      val = `abap.buildDbTableName(`;
    }

    const dyn = node.findDirectExpression(abaplint.Expressions.Dynamic);
    if (dyn) {
      if (concat.startsWith("('")) {
        val += `"` + concat.substring(2, concat.length - 2).toLowerCase() + `"`;
      } else {
        const foo = traversal.traverse(dyn.findDirectExpression(abaplint.Expressions.FieldChain));
        val += foo.getCode() + ".get().trimEnd().toLowerCase()";
      }
    } else {
      val += `"` + concat.toLowerCase() + `"`;
    }

    if (this.prefix === true) {
      val += `)`;
    }

    return chunk.appendString(val);
  }

}