import {Context} from "../context";
import {DatabaseRows, SelectDatabaseOptions, SelectRuntimeOptions} from "../db/db";
import {FieldSymbol, HashedTable, Structure, Table} from "../types";

export async function select(target: Structure | Table | HashedTable | FieldSymbol,
                             input: SelectDatabaseOptions,
                             runtimeOptions: SelectRuntimeOptions,
                             context: Context) {
  const {rows: rows} = await context.defaultDB().select(input);

  if (target instanceof FieldSymbol) {
    if (target.isAssigned() === false) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
      // @ts-ignore
    target = target.getPointer();
  }

  if (runtimeOptions?.appending !== true) {
    if (Array.isArray(target)) {
      target.forEach(f => f.clear());
    } else {
      target?.clear();
    }
  }

  if (rows.length === 0) {
      // @ts-ignore
    abap.builtin.sy.get().dbcnt.set(0);
      // @ts-ignore
    abap.builtin.sy.get().subrc.set(4);
    return;
  }

  rowsToTarget(target, rows);

  if (target === undefined && rows.length === 1) {
      // @ts-ignore
    abap.builtin.sy.get().dbcnt.set(Object.values(rows[0])[0]);
  } else {
      // @ts-ignore
    abap.builtin.sy.get().dbcnt.set(rows.length);
  }
    // @ts-ignore
  abap.builtin.sy.get().subrc.set(0);
}

export function rowsToTarget(target: Structure | Table | HashedTable | FieldSymbol, rows: DatabaseRows) {
  if (target instanceof Structure) {
    const result: any = {};
    for (const column in rows[0]) {
      if (rows[0][column] === null || target.get()[column] === undefined) {
        continue;
      }
      result[column] = target.get()[column].clone().set(rows[0][column]);
    }
      // @ts-ignore
    abap.statements.moveCorresponding(new Structure(result), target);
  } else if (target instanceof Table || target instanceof HashedTable) {
    for (const row of rows) {
      const targetRow = target.getRowType().clone();
      if (targetRow instanceof Structure) {
        for (let columnName in row) {
          columnName = columnName.toLowerCase();
          if (row[columnName] === null) {
            targetRow.get()[columnName]?.clear();
            continue;
          }
            // @ts-ignore
          targetRow.get()[columnName]?.set(row[columnName]);
        }
      } else {
        const columnName = Object.keys(row)[0];
        targetRow.set(row[columnName]);
      }

        // @ts-ignore
      abap.statements.insertInternal({table: target, data: targetRow, noClone: true});
    }
  } else if (Array.isArray(target)) {
    for (let index = 0; index < target.length; index++) {
      const element = target[index];
      element.set(rows[0][Object.keys(rows[0])[index]]);
    }
  } else if (target !== undefined) {
      // its a simple type
    target.set(rows[0][Object.keys(rows[0])[0]]);
  }
}