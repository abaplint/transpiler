import * as abaplint from "@abaplint/core";
import {Traversal} from "../traversal";

type Kid = abaplint.Nodes.ExpressionNode | abaplint.Nodes.TokenNode;

/** where EXPORT, IMPORT and DELETE FROM keep a data cluster: the key in the runtime's store (code of a string), or
 * the byte string of DATA BUFFER (code of the data object), or why it is not supported */
export interface IClusterMedium {
  key?: string;
  buffer?: string;
  unsupported?: string;
}

function isToken(n: Kid | undefined, text: string): boolean {
  return n instanceof abaplint.Nodes.TokenNode && n.concatTokens().toUpperCase() === text;
}

/** the medium, from the children of the statement after the keyword TO or FROM */
export function clusterMedium(rest: readonly Kid[], traversal: Traversal): IClusterMedium {
  const idCode = (): string => {
    const at = rest.findIndex(n => isToken(n, "ID"));
    const id = at >= 0 ? rest[at + 1] : undefined;
    return id ? traversal.traverse(id).getCode() + ".get().toString().trimEnd()" : `""`;
  };
  const first = rest[0]?.concatTokens().toUpperCase();
  if (first === "MEMORY") {
    return {key: `"MEMORY|" + ` + idCode()};
  } else if (first === "DATABASE" || first === "SHARED") {
    // DATABASE dbtab(ar) ... / SHARED MEMORY|BUFFER dbtab(ar) ...
    const at = first === "DATABASE" ? 1 : 2;
    const table = rest[at]?.concatTokens().toUpperCase();
    const area = rest[at + 2]?.concatTokens().toUpperCase();
    const kind = first === "DATABASE" ? "DATABASE" : "SHARED";
    return {key: JSON.stringify(kind + "|" + table + "|" + area + "|") + " + " + idCode()};
  } else if (first === "DATA" && isToken(rest[1], "BUFFER") && rest[2]) {
    return {buffer: traversal.traverse(rest[2]).getCode()};
  }
  return {unsupported: rest.map(n => n.concatTokens()).join(" ")};
}

/** the parameters of EXPORT p1 = dobj1 p2 FROM dobj2 dobj3 (link "FROM" or "="), or of
 * IMPORT p1 = dobj1 p2 TO dobj2 dobj3 (link "TO", "INTO" or "="): parameter name in upper case and the code of the data object */
export function clusterParameters(list: readonly Kid[], links: string[],
                                  traversal: Traversal): {pairs: [string, string][], unsupported?: string} {
  const pairs: [string, string][] = [];
  for (let i = 0; i < list.length; i++) {
    const k = list[i];
    const next = list[i + 1];
    const link = next instanceof abaplint.Nodes.TokenNode ? next.concatTokens().toUpperCase() : "";
    if (links.includes(link) && list[i + 2]) {
      pairs.push([k.concatTokens().toUpperCase(), traversal.traverse(list[i + 2]).getCode()]);
      i += 2;
    } else if (k instanceof abaplint.Nodes.ExpressionNode
        && (k.get() instanceof abaplint.Expressions.Target
        || k.get() instanceof abaplint.Expressions.Source
        || k.get() instanceof abaplint.Expressions.FieldSub
        || k.get() instanceof abaplint.Expressions.FieldSymbol
        || k.get() instanceof abaplint.Expressions.ComponentChainSimple)
        && /^(<[\w/]+>|[\w/]+)$/.test(k.concatTokens())) {
      // the data object stands for itself, under its own name
      const name = k.concatTokens();
      const code = k.get() instanceof abaplint.Expressions.Target || k.get() instanceof abaplint.Expressions.Source
        ? traversal.traverse(k).getCode()
        : traversal.prefixAndName(k.getFirstToken(), traversal.getFilename()).replace("~", "$");
      pairs.push([name.toUpperCase(), code]);
    } else {
      return {pairs, unsupported: k.concatTokens()};
    }
  }
  return {pairs};
}

export function clusterValues(pairs: [string, string][]): string {
  return "{" + pairs.map(([name, code]) => JSON.stringify(name) + ": " + code).join(", ") + "}";
}
