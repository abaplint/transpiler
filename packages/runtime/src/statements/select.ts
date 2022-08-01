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
        throw "select, fs not assigned";
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
        result[column] = clone(target.get()[column]).set(rows[0][column]);
      }
      target.set(new Structure(result));
    } else if (target instanceof Table) {
      for (const row of rows) {
        const targetRow = clone(target.getRowType());
        for (let columnName in row) {
          columnName = columnName.toLowerCase();
          // todo, non structured table = table with simple rows
          // @ts-ignore
          targetRow.get()[columnName]?.set(row[columnName]);
        }

        // @ts-ignore
        abap.statements.insertInternal({table: target, data: targetRow});
      }
    } else if (target !== undefined) {
      throw new Error("Runtime, SELECT todo");
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