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

    const {rows: res} = this.context.db.select({select});

    if (target instanceof FieldSymbol) {
      // @ts-ignore
      target = target.getPointer();
    }

    target.clear();
    if (target instanceof Structure) {
      if (res.length === 0) {
        // @ts-ignore
        abap.builtin.sy.get().subrc.set(4);
      } else {
        const result: any = {};
        for (const column in res[0]) {
          result[column] = clone(target.get()[column]).set(res[0][column]);
        }
        target.set(new Structure(result));
        // @ts-ignore
        abap.builtin.sy.get().subrc.set(0);
      }
    } else if (target instanceof Table) {
      if (res.length === 0) {
        // @ts-ignore
        abap.builtin.sy.get().subrc.set(4);
      } else {
        for (const row of res) {
          const targetRow = clone(target.getRowType());
          for (const columnName in row) {
            // @ts-ignore
            targetRow.get()[columnName]?.set(row[columnName]);
          }

          // @ts-ignore
          abap.statements.insertInternal({table: target, data: targetRow});
        }

        // @ts-ignore
        abap.builtin.sy.get().subrc.set(0);
      }
    } else {
      throw new Error("Runtime, SELECT todo");
    }
  }
}