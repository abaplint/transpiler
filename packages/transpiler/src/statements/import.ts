import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {clusterMedium, clusterParameters, clusterValues} from "./_cluster";

export class ImportTranspiler implements IStatementTranspiler {

  /** IMPORT p1 = dobj1 p2 TO dobj2 ... FROM MEMORY [ID id] | DATABASE dbtab(ar) ID id | DATA BUFFER xstr | SHARED ... */
  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const kids = node.getChildren();
    const from = kids.findIndex(k => k instanceof abaplint.Nodes.TokenNode && k.concatTokens().toUpperCase() === "FROM");
    const params = clusterParameters(kids.slice(1, from < 0 ? 1 : from), ["TO", "INTO", "="], traversal);
    const medium = clusterMedium(kids.slice(from + 1), traversal);
    if (from < 0 || params.unsupported !== undefined || medium.unsupported !== undefined) {
      const what = params.unsupported ?? medium.unsupported ?? node.concatTokens();
      return new Chunk().append(`throw new Error(${JSON.stringify("IMPORT, not supported, transpiler: " + what)});`, node, traversal);
    }
    const targets = clusterValues(params.pairs);
    if (medium.buffer !== undefined) {
      return new Chunk().append(`await abap.statements.importBuffer(${medium.buffer}, ${targets});`, node, traversal);
    }
    return new Chunk().append(`await abap.statements.importCluster(${medium.key}, ${targets});`, node, traversal);
  }

}
