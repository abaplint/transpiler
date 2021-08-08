import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

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