import * as abaplint from "@abaplint/core";
import {Traversal} from "../traversal";

// EXPORT ... TO / IMPORT ... FROM a data cluster: the part after TO / FROM,
// read off the statement's children. Only SHARED MEMORY and SHARED BUFFER
// are transpiled; for every other medium undefined is returned and the
// statement keeps its runtime "transpiler todo" error.
export interface IDataClusterMedium {
  medium: "SHARED MEMORY" | "SHARED BUFFER";
  table: string;
  area: string;
  id?: string;
  client?: string;
}

export function sharedMedium(node: abaplint.Nodes.StatementNode, keyword: "TO" | "FROM", traversal: Traversal): IDataClusterMedium | undefined {
  const children = node.getChildren();
  let at = -1;
  for (let i = 0; i < children.length - 1; i++) {
    const here = children[i];
    const next = children[i + 1];
    if (here instanceof abaplint.Nodes.TokenNode && here.getFirstToken().getStr().toUpperCase() === keyword
        && next instanceof abaplint.Nodes.TokenNode && next.getFirstToken().getStr().toUpperCase() === "SHARED") {
      at = i + 1;
      break;
    }
  }
  if (at < 0) {
    return undefined;
  }
  const kind = children[at + 1]?.getFirstToken().getStr().toUpperCase();
  if (kind !== "MEMORY" && kind !== "BUFFER") {
    return undefined;
  }
  const tableNode = children[at + 2];
  const areaNode = children[at + 4];
  if (!(tableNode instanceof abaplint.Nodes.ExpressionNode) || areaNode === undefined) {
    return undefined;
  }
  const ret: IDataClusterMedium = {
    medium: kind === "MEMORY" ? "SHARED MEMORY" : "SHARED BUFFER",
    table: tableNode.concatTokens().toUpperCase(),
    area: areaNode.getFirstToken().getStr().toUpperCase(),
  };
  // after the "( ar )": the additions in any order, ID / CLIENT followed by
  // a Source; FROM wa (EXPORT) and TO wa (IMPORT) name the dbtab's work area,
  // whose extra fields have no place in this store and are not read
  for (let i = at + 6; i < children.length - 1; i++) {
    const here = children[i];
    const next = children[i + 1];
    if (!(here instanceof abaplint.Nodes.TokenNode) || !(next instanceof abaplint.Nodes.ExpressionNode)) {
      continue;
    }
    const word = here.getFirstToken().getStr().toUpperCase();
    if (word === "ID") {
      ret.id = traversal.traverse(next).getCode();
    } else if (word === "CLIENT") {
      ret.client = traversal.traverse(next).getCode();
    }
  }
  if (ret.id === undefined) {
    return undefined;
  }
  return ret;
}

export function mediumOptions(medium: IDataClusterMedium): string {
  return `medium: ${JSON.stringify(medium.medium)}, table: ${JSON.stringify(medium.table)}, `
    + `area: ${JSON.stringify(medium.area)}, id: ${medium.id}`
    + (medium.client === undefined ? "" : `, client: ${medium.client}`);
}
