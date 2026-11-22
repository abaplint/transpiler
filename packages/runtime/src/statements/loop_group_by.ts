import {compareText, gt, lt} from "../compare";
import {FieldSymbol, Integer, Structure} from "../types";
import {ABAP} from "..";

declare const abap: ABAP;

/** a component of a structured group key: a value, or GROUP SIZE / GROUP INDEX */
export type GroupKeyComponent = [name: string, value: any] | [name: string, value: "SIZE" | "INDEX"];

export interface IGroup {
  /** the group key, for a structured key a structure with GROUP SIZE and GROUP INDEX filled */
  key: any,
  /** the first row of the group, the representative */
  representative: any,
  members: any[],
  /** sy-tabix of each member in the grouped table */
  tabix: number[],
  /** GROUP INDEX, the number of the group in order of the first occurrence of its key, also when sorted */
  index: number,
}

function deref(value: any): any {
  return value instanceof FieldSymbol ? value.getPointer() : value;
}

/** the value of a key as a string, equal keys give equal strings */
function serialize(value: any, skip?: Set<string>): string {
  value = deref(value);
  if (value instanceof Structure) {
    const parts: string[] = [];
    for (const [name, component] of Object.entries(value.get())) {
      if (skip?.has(name) !== true) {
        parts.push(name + ":" + serialize(component));
      }
    }
    return "{" + parts.join(",") + "}";
  }
  const raw = value?.get?.();
  return typeof raw === "bigint" ? raw.toString() + "n" : JSON.stringify(raw);
}

/** LOOP AT itab ... GROUP BY key: collects the rows into groups, in order of the first occurrence
 *  of each key, a structured key is compared without its GROUP SIZE and GROUP INDEX components */
export class GroupBy {
  private readonly groups: IGroup[] = [];
  private readonly lookup = new Map<string, IGroup>();
  private size: string | undefined = undefined;
  private index: string | undefined = undefined;

  /** key is the value of a simple key, or the components of a structured key */
  public add(row: any, key: any | GroupKeyComponent[]): void {
    let value: any;
    if (Array.isArray(key)) {
      const fields: {[name: string]: any} = {};
      for (const [name, component] of key) {
        if (component === "SIZE" || component === "INDEX") {
          fields[name] = new Integer();
          if (component === "SIZE") {
            this.size = name;
          } else {
            this.index = name;
          }
        } else {
          fields[name] = deref(component).clone();
        }
      }
      value = new Structure(fields);
    } else {
      value = deref(key).clone();
    }

    const skip = new Set<string>();
    if (this.size !== undefined) {
      skip.add(this.size);
    }
    if (this.index !== undefined) {
      skip.add(this.index);
    }
    const serialized = serialize(value, skip);

    let group = this.lookup.get(serialized);
    if (group === undefined) {
      group = {key: value, representative: row, members: [], tabix: [], index: this.groups.length + 1};
      this.lookup.set(serialized, group);
      this.groups.push(group);
    }
    group.members.push(row);
    group.tabix.push(abap.builtin.sy.get().tabix.get());
  }

  /** the groups in the order of the group loop, with GROUP SIZE and GROUP INDEX set,
   *  text: AS TEXT, character-like key components are compared by the text environment */
  public getGroups(order?: "ASCENDING" | "DESCENDING", text = false): IGroup[] {
    const ret = [...this.groups];
    if (order !== undefined) {
      const direction = order === "ASCENDING" ? 1 : -1;
      // stable, groups with equal keys keep the order of their first occurrence
      ret.sort((a, b) => direction * this.compare(a.key, b.key, text));
    }
    for (const group of ret) {
      if (this.size !== undefined) {
        group.key.get()[this.size].set(group.members.length);
      }
      if (this.index !== undefined) {
        group.key.get()[this.index].set(group.index);
      }
    }
    return ret;
  }

  /** the group loop sets sy-tabix to the number of the pass with a group target,
   *  else to the row number of the representative */
  public setTabix(group: IGroup, pass: number, representative: boolean): void {
    abap.builtin.sy.get().tabix.set(representative ? group.tabix[0] : pass);
  }

  private compare(a: any, b: any, text: boolean): number {
    if (a instanceof Structure) {
      for (const name of Object.keys(a.get())) {
        if (name === this.size || name === this.index) {
          continue;
        }
        const c = this.compare(a.get()[name], b.get()[name], text);
        if (c !== 0) {
          return c;
        }
      }
      return 0;
    }
    if (text) {
      return compareText(a, b);
    }
    return lt(a, b) ? -1 : gt(a, b) ? 1 : 0;
  }
}

export function groupBy(): GroupBy {
  return new GroupBy();
}

/** LOOP AT GROUP group ... [WHERE ...]: the members of the current group, the rows of the grouped table */
export async function* loopGroup(group: IGroup | undefined,
                                 options?: {where?: (i: any) => Promise<boolean>}): AsyncGenerator<any, void, unknown> {
  if (group === undefined) {
    throw new Error("LOOP AT GROUP, no group");
  }
  const outerTabix = abap.builtin.sy.get().tabix.get();
  let entered = false;
  try {
    for (let i = 0; i < group.members.length; i++) {
      const current = group.members[i];
      if (options?.where) {
        const row = current instanceof Structure ? current.get() : {table_line: current};
        if (await options.where(row) === false) {
          continue;
        }
      }
      abap.builtin.sy.get().tabix.set(group.tabix[i]);
      entered = true;
      yield current;
    }
  } finally {
    abap.builtin.sy.get().subrc.set(entered ? 0 : 4);
    abap.builtin.sy.get().tabix.set(outerTabix);
  }
}
