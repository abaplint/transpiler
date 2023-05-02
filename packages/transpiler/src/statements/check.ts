import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";
import {ReturnTranspiler} from "./index.js";

export class CheckTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const cond = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Cond));

    const ret = new Chunk();
    ret.append("if (!(", node, traversal);
    ret.appendChunk(cond);
    ret.appendString(")) {\n");
    if (traversal.isInsideLoop(node)) {
      ret.appendString("continue;");
    } else {
      ret.appendChunk(new ReturnTranspiler().transpile(node, traversal));
    }
    ret.append("\n}", node.getLastToken(), traversal);
    return ret;
  }

}