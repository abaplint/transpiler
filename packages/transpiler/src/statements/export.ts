import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {clusterMedium, clusterParameters, clusterValues} from "./_cluster";

export class ExportTranspiler implements IStatementTranspiler {

  /** EXPORT p1 = dobj1 p2 FROM dobj2 ... TO MEMORY [ID id] | DATABASE dbtab(ar) ID id | DATA BUFFER xstr | SHARED ... */
  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const kids = node.getChildren();
    const to = kids.findIndex(k => k instanceof abaplint.Nodes.TokenNode && k.concatTokens().toUpperCase() === "TO");
    const params = clusterParameters(kids.slice(1, to < 0 ? 1 : to), ["FROM", "="], traversal);
    const medium = clusterMedium(kids.slice(to + 1), traversal);
    if (to < 0 || params.unsupported !== undefined || medium.unsupported !== undefined) {
      const what = params.unsupported ?? medium.unsupported ?? node.concatTokens();
      return new Chunk().append(`throw new Error(${JSON.stringify("EXPORT, not supported, transpiler: " + what)});`, node, traversal);
    }
    const values = clusterValues(params.pairs);
    if (medium.buffer !== undefined) {
      return new Chunk().append(`abap.statements.exportBuffer(${medium.buffer}, ${values});`, node, traversal);
    }
    return new Chunk().append(`abap.statements.exportCluster(${medium.key}, ${values});`, node, traversal);
  }

}
