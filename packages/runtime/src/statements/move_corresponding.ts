import {FieldSymbol, HashedTable, Structure, Table} from "../types";
import {insertInternal} from "./insert_internal";

type Corresponding = Structure | FieldSymbol | Table | HashedTable;

export function moveCorresponding(source: Corresponding, target: Corresponding): Corresponding {
  if (source instanceof FieldSymbol) {
    if (source.isAssigned() === false) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return moveCorresponding(source.getPointer(), target);
  }
  if (target instanceof FieldSymbol) {
    if (target.isAssigned() === false) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return moveCorresponding(source, target.getPointer());
  }

  const isTable = (t: Corresponding) => t instanceof Table || t instanceof HashedTable;
  if (isTable(source) && isTable(target)) {
    const src = source as Table | HashedTable;
    const tgt = target as Table | HashedTable;
    if (src.hasHeaderLine() === false && tgt.hasHeaderLine() === false) {
      // internal tables without header lines: the target gets the source's lines, component by component
      tgt.clear();
      for (const row of src.array()) {
        const line = tgt.getRowType().clone();
        if (line instanceof Structure && row instanceof Structure) {
          moveCorresponding(row, line);
        } else {
          line.set(row);
        }
        insertInternal({table: tgt, data: line});
      }
      return tgt;
    }
  }

  // tables with header lines: their header lines
  const s = isTable(source) ? (source as Table | HashedTable).getHeader() as Structure : source as Structure;
  const d = isTable(target) ? (target as Table | HashedTable).getHeader() as Structure : target as Structure;
  for (const n in s.get()) {
    if (d.get()[n] instanceof Structure) {
      moveCorresponding(s.get()[n], d.get()[n]);
    } else {
      d.get()[n]?.set(s.get()[n]);
    }
  }

  return target;
}