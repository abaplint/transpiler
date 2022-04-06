import {clone} from "../clone";
import {Context} from "../context";
import {FieldSymbol, Structure, Table} from "../types";

export class SelectDatabase {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public select(target: Structure | Table | FieldSymbol, select: string) {
    if (this.context.db === undefined) {
      throw new Error("Runtime, database not initialized");
    }

    const {rows: rows} = this.context.db.select({select});

    if (target instanceof FieldSymbol) {
      if (target.isAssigned() === false) {
        throw "select, fs not assigned";
      }
      // @ts-ignore
      target = target.getPointer();
    }
    target.clear();

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
        for (const columnName in row) {
          // @ts-ignore
          targetRow.get()[columnName]?.set(row[columnName]);
        }

        // @ts-ignore
        abap.statements.insertInternal({table: target, data: targetRow});
      }
    } else {
      throw new Error("Runtime, SELECT todo");
    }

    // @ts-ignore
    abap.builtin.sy.get().subrc.set(0);
  }
}