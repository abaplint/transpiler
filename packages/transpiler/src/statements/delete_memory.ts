import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {dataClusterMedium, mediumOptions} from "./_data_cluster";

export class DeleteMemoryTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const medium = dataClusterMedium(node, "FROM", traversal);
    if (medium === undefined) {
      return new Chunk(`throw new Error("DeleteMemory, not supported, transpiler");`);
    }
    return new Chunk(`abap.statements.deleteCluster({${mediumOptions(medium)}});`);
  }

}