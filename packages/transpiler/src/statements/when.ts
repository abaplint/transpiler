import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {SourceTranspiler} from "../expressions/index.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class WhenTranspiler implements IStatementTranspiler {
  private readonly u: string;

  public constructor(u: string) {
    this.u = u;
  }

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    let ret = "";

    for (const s of node.findAllExpressions(abaplint.Expressions.Source)) {
      const source = new SourceTranspiler().transpile(s, traversal).getCode();
      if (ret !== "") {
        ret += " || ";
      }
      ret += "abap.compare.eq(" + this.u + ", " + source + ")";
    }

    return new Chunk().append(ret, node, traversal);
  }

}