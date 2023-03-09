import {clone} from "../clone";
import {Context} from "../context";
import {SelectDatabaseOptions, SelectRuntimeOptions} from "../db/db";
import {FieldSymbol, Structure, Table} from "../types";

export class SelectDatabase {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public async select(target: Structure | Table | FieldSymbol, input: SelectDatabaseOptions, runtimeOptions?: SelectRuntimeOptions) {
    const {rows: rows} = await this.context.defaultDB().select(input);

    if (target instanceof FieldSymbol) {
      if (target.isAssigned() === false) {
        throw new Error("GETWA_NOT_ASSIGNED");
      }
      // @ts-ignore
      target = target.getPointer();
    }

    if (runtimeOptions?.appending !== true) {
      target?.clear();
    }

    if (rows.length === 0) {
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(4);
      return;
    }

    if (target instanceof Structure) {
      const result: any = {};
      for (const column in rows[0]) {
        if (rows[0][column] === null) {
          continue;
        }
        result[column] = clone(target.get()[column]).set(rows[0][column]);
      }
      // @ts-ignore
      abap.statements.moveCorresponding(new Structure(result), target);
    } else if (target instanceof Table) {
      for (const row of rows) {
        const targetRow = clone(target.getRowType());
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
        abap.statements.insertInternal({table: target, data: targetRow});
      }
    } else if (target !== undefined) {
      // its a simple type
      target.set(rows[0][Object.keys(rows[0])[0]]);
    }

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
}