import {eq} from "../compare";
import {add} from "../operators";
import {Character, Date, DecFloat34, FieldSymbol, Float, Integer, Integer8, Numc, Packed, Structure, Time} from "../types";

// Control level processing, AT NEW / AT END OF / AT FIRST / AT LAST and SUM inside LOOP AT itab

type Leaf = {path: string, value: any};

/** the elementary components of a row in their order, "a-b" for a component of a substructure; fields of an
 *  include that the structure also links at its top level are listed once */
function leaves(row: any): Leaf[] {
  const ret: Leaf[] = [];
  const seen = new Set<any>();
  const walk = (value: any, path: string) => {
    if (value instanceof FieldSymbol) {
      value = value.getPointer();
    }
    if (value instanceof Structure) {
      for (const [name, component] of Object.entries(value.get())) {
        walk(component, path === "" ? name : path + "-" + name);
      }
    } else if (!seen.has(value)) {
      seen.add(value);
      ret.push({path, value});
    }
  };
  walk(row, "");
  return ret;
}

/** how many leading elementary components the control key covers: up to and including comp (a component or a
 *  substructure), every component for table_line, none for "" (AT FIRST, AT LAST) */
function keyLength(list: Leaf[], comp: string): number {
  const name = comp.trim().toLowerCase();
  if (name === "") {
    return 0;
  } else if (name === "table_line") {
    return list.length;
  }
  let last = -1;
  list.forEach((l, i) => {
    if (l.path === name || l.path.startsWith(name + "-")) {
      last = i;
    }
  });
  if (last < 0) {
    throw new Error("ITAB_ILLEGAL_COMPONENT");
  }
  return last + 1;
}

/** true when two rows have the same content in comp and in every component left of it */
export function atSameKey(a: any, b: any, comp: string): boolean {
  if (a instanceof FieldSymbol) {
    a = a.getPointer();
  }
  if (b instanceof FieldSymbol) {
    b = b.getPointer();
  }
  if (!(a instanceof Structure) || comp.trim().toLowerCase() === "table_line") {
    return eq(a, b);
  }
  const la = leaves(a);
  const lb = leaves(b);
  const count = keyLength(la, comp);
  for (let i = 0; i < count; i++) {
    if (eq(la[i].value, lb[i].value) === false) {
      return false;
    }
  }
  return true;
}

/** the work area of LOOP ... INTO when an AT block is entered: components right of the control key are set to "*"
 *  when character-like and flat, else to their initial value; comp "" for AT FIRST and AT LAST */
export function atMask(workArea: any, comp: string) {
  const list = leaves(workArea);
  const count = keyLength(list, comp);
  for (const {value} of list.slice(count)) {
    if (value instanceof Character) {
      value.set("*".repeat(value.getLength()));
    } else if (value instanceof Numc || value instanceof Date || value instanceof Time) {
      // set without the conversion rules, which would not keep a "*"
      (value as any).value = "*".repeat(value instanceof Numc ? value.getLength() : value instanceof Date ? 8 : 6);
    } else if (typeof value?.clear === "function") {
      value.clear();
    }
  }
}

function isNumeric(value: any) {
  return value instanceof Integer || value instanceof Integer8 || value instanceof Packed
    || value instanceof Float || value instanceof DecFloat34;
}

/** SUM: the numeric components of the rows of the current control level, put into the work area. rows: the rows the
 *  LOOP reads, in its order; current: the row of this pass; comp: the control key of the AT block, undefined in AT FIRST,
 *  AT LAST and outside AT (then all rows are added) */
export function sum(workArea: any, rows: readonly any[], current: any, comp?: string) {
  let from = 0;
  let to = rows.length - 1;
  if (comp !== undefined && comp.trim() !== "") {
    const at = rows.indexOf(current);
    if (at >= 0) {
      from = at;
      to = at;
      while (from > 0 && atSameKey(rows[from - 1], current, comp)) {
        from--;
      }
      while (to < rows.length - 1 && atSameKey(rows[to + 1], current, comp)) {
        to++;
      }
    }
  }
  const target = leaves(workArea);
  const numeric = target.map((l, i) => isNumeric(l.value) ? i : -1).filter(i => i >= 0);
  for (const i of numeric) {
    target[i].value.clear();
  }
  for (let r = from; r <= to; r++) {
    const values = leaves(rows[r]);
    for (const i of numeric) {
      target[i].value.set(add(target[i].value, values[i].value));
    }
  }
}
