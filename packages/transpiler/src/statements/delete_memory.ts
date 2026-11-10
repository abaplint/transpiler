import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {clusterMedium} from "./_cluster";

export class DeleteMemoryTranspiler implements IStatementTranspiler {

  /** DELETE FROM MEMORY ID id | DELETE FROM SHARED MEMORY|BUFFER dbtab(ar) ID id: sy-subrc 4 when there was no cluster */
  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const kids = node.getChildren();
    const from = kids.findIndex(k => k instanceof abaplint.Nodes.TokenNode && k.concatTokens().toUpperCase() === "FROM");
    const medium = clusterMedium(kids.slice(from + 1), traversal);
    if (from < 0 || medium.key === undefined) {
      throw new Error("DeleteMemoryTranspiler, not supported: " + node.concatTokens());
    }
    return new Chunk().append(`abap.statements.deleteCluster(${medium.key});`, node, traversal);
  }

}
