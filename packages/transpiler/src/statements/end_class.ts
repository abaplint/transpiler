import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IStatementTranspiler} from "./_statement_transpiler";

export class EndClassTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const def = traversal.getClassDefinition(node.getFirstToken());
    let ret = "}\n";
    ret += traversal.registerClassOrInterface(def);
    return new Chunk(ret);
  }

}