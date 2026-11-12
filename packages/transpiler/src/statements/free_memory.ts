import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class FreeMemoryTranspiler implements IStatementTranspiler {

  /** FREE MEMORY [ID id]: deletes the cluster of the ID, or every cluster of the ABAP memory */
  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const id = node.findDirectExpression(abaplint.Expressions.Source);
    const key = id ? `"MEMORY|" + ` + traversal.traverse(id).getCode() + ".get().toString().trimEnd()" : "";
    return new Chunk().append(`abap.statements.freeMemory(${key});`, node, traversal);
  }

}
