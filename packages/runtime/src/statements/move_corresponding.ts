import {FieldSymbol, HashedTable, Structure, Table} from "../types";
import {insertInternal} from "./insert_internal";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IMoveCorrespondingOptions {
  /** MOVE-CORRESPONDING itab TO jtab KEEPING TARGET LINES: the rows are added to the rows already there */
  keepingTargetLines?: boolean,
  /** EXPANDING NESTED TABLES: components that are tables are moved row by row, by component name, too */
  expandingNestedTables?: boolean,
}

const isTable = (x: any) => x instanceof Table || x instanceof HashedTable;

type Operand = Structure | FieldSymbol | Table | HashedTable;

export function moveCorresponding(source: Operand, target: Operand,
                                  options?: IMoveCorrespondingOptions): any {
  if (source instanceof FieldSymbol) {
    if (source.isAssigned() === false) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return moveCorresponding(source.getPointer(), target, options);
  }
  if (target instanceof FieldSymbol) {
    if (target.isAssigned() === false) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return moveCorresponding(source, target.getPointer(), options);
  }

  if (isTable(source) && isTable(target)) {
    // the target is emptied (unless KEEPING TARGET LINES), and gets one row per source row: an initial row with the
    // components of the same name moved over, inserted as INSERT ... INTO TABLE does
    const from = source as Table | HashedTable;
    const to = target as Table | HashedTable;
    const rows = from === to ? [...from.array()] : from.array();
    const subrc = abap.builtin.sy.get().subrc.get();
    if (options?.keepingTargetLines !== true) {
      to.clear();
    }
    for (const row of rows) {
      const fresh = to.getRowType().clone();
      if (typeof (fresh as any).clear === "function") {
        (fresh as any).clear();
      }
      if (fresh instanceof Structure && row instanceof Structure) {
        moveCorresponding(row, fresh, options);
      } else {
        (fresh as any).set(row);
      }
      insertInternal({table: to, data: fresh});
    }
    // MOVE-CORRESPONDING sets no sy-subrc
    abap.builtin.sy.get().subrc.set(subrc);
    return target;
  }

  // a structure, or a table with header line (its header line)
  for (const n in (source as any).get()) {
    const to = (target as any).get()[n];
    const from = (source as any).get()[n];
    if (to instanceof Structure) {
      moveCorresponding(from, to, options);
    } else if (options?.expandingNestedTables === true && isTable(to) && isTable(from)) {
      moveCorresponding(from, to, options);
    } else {
      to?.set(from);
    }
  }

  return target;
}
