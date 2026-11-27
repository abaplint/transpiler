import * as abaplint from "@abaplint/core";
import {Traversal} from "../traversal";

// EXPORT ... TO / IMPORT ... FROM / DELETE FROM a data cluster: the part after
// TO / FROM, read off the statement's children. MEMORY, DATABASE, SHARED
// MEMORY, SHARED BUFFER and DATA BUFFER are transpiled; for every other
// medium undefined is returned and the statement keeps its runtime error.
export interface IDataClusterMedium {
  medium: "MEMORY" | "DATABASE" | "SHARED MEMORY" | "SHARED BUFFER" | "DATA BUFFER";
  table?: string;
  area?: string;
  id?: string;
  client?: string;
  buffer?: string;
}

function word(node: abaplint.Nodes.ExpressionNode | abaplint.Nodes.TokenNode | undefined): string {
  return node instanceof abaplint.Nodes.TokenNode ? node.getFirstToken().getStr().toUpperCase() : "";
}

export function dataClusterMedium(node: abaplint.Nodes.StatementNode, keyword: "TO" | "FROM", traversal: Traversal): IDataClusterMedium | undefined {
  const children = node.getChildren();
  const at = children.findIndex(c => word(c) === keyword) + 1;
  if (at === 0) {
    return undefined;
  }
  let ret: IDataClusterMedium;
  let additions: number;
  if (word(children[at]) === "MEMORY") {
    ret = {medium: "MEMORY"};
    additions = at + 1;
  } else if (word(children[at]) === "DATA" && word(children[at + 1]) === "BUFFER"
      && children[at + 2] instanceof abaplint.Nodes.ExpressionNode) {
    return {medium: "DATA BUFFER", buffer: traversal.traverse(children[at + 2]).getCode()};
  } else {
    // DATABASE dbtab(ar) ... / SHARED MEMORY|BUFFER dbtab(ar) ...
    let tableAt: number;
    if (word(children[at]) === "DATABASE") {
      ret = {medium: "DATABASE"};
      tableAt = at + 1;
    } else if (word(children[at]) === "SHARED" && (word(children[at + 1]) === "MEMORY" || word(children[at + 1]) === "BUFFER")) {
      ret = {medium: word(children[at + 1]) === "MEMORY" ? "SHARED MEMORY" : "SHARED BUFFER"};
      tableAt = at + 2;
    } else {
      return undefined;
    }
    const tableNode = children[tableAt];
    const areaNode = children[tableAt + 2];
    if (!(tableNode instanceof abaplint.Nodes.ExpressionNode) || areaNode === undefined) {
      return undefined;
    }
    ret.table = tableNode.concatTokens().toUpperCase();
    ret.area = areaNode.getFirstToken().getStr().toUpperCase();
    additions = tableAt + 4;
  }
  // the additions in any order, ID / CLIENT followed by a Source; FROM wa
  // (EXPORT) and TO wa (IMPORT) name the dbtab's work area, whose extra
  // fields have no place in this store and are not read
  for (let i = additions; i < children.length - 1; i++) {
    const here = children[i];
    const next = children[i + 1];
    if (!(here instanceof abaplint.Nodes.TokenNode) || !(next instanceof abaplint.Nodes.ExpressionNode)) {
      continue;
    }
    if (word(here) === "ID") {
      ret.id = traversal.traverse(next).getCode();
    } else if (word(here) === "CLIENT") {
      ret.client = traversal.traverse(next).getCode();
    } else if (word(here) === "USING") {
      return undefined;
    }
  }
  if (ret.medium !== "MEMORY" && ret.id === undefined) {
    return undefined;
  }
  return ret;
}

export function mediumOptions(medium: IDataClusterMedium): string {
  return `medium: ${JSON.stringify(medium.medium)}`
    + (medium.table === undefined ? "" : `, table: ${JSON.stringify(medium.table)}`)
    + (medium.area === undefined ? "" : `, area: ${JSON.stringify(medium.area)}`)
    + (medium.id === undefined ? "" : `, id: ${medium.id}`)
    + (medium.client === undefined ? "" : `, client: ${medium.client}`)
    + (medium.buffer === undefined ? "" : `, buffer: ${medium.buffer}`);
}
