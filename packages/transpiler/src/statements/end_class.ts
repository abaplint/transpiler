import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {Traversal} from "../traversal.js";
import {IStatementTranspiler} from "./_statement_transpiler.js";

export class EndClassTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const def = traversal.getClassDefinition(node.getFirstToken());
    let ret = "}\n";
    ret += traversal.registerClassOrInterface(def);
    return new Chunk().append(ret, node, traversal);
  }

}