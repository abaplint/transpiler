import {clone} from "../clone";
import {Structure, Table} from "../types";

export function select(target: Structure | Table, select: string) {
  if (this.db === undefined) {
    throw new Error("Runtime, database not initialized");
  }

  let res: undefined | any = undefined;
  try {
    res = this.db.exec(select);
  } catch (error) {
    // @ts-ignore
    if (abap.Classes["CX_SY_DYNAMIC_OSQL_SEMANTICS"] !== undefined) {
      // @ts-ignore
      throw new abap.Classes["CX_SY_DYNAMIC_OSQL_SEMANTICS"]();
    }
    throw error;
  }

  target.clear();
  if (target instanceof Structure) {
    if (res[0]?.values[0] === undefined) {
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(4);
    } else {
      const columns: string[] = res[0].columns;
      const values: any[] = res[0].values[0];
      const result: any = {};
      for (const c of columns) {
        result[c] = clone(target.get()[c]).set(values.shift());
      }
      target.set(new Structure(result));
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(0);
    }
  } else if (target instanceof Table) {
    if (res[0]?.values[0] === undefined) {
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(4);
    } else {
      for (const selectRow of res[0].values) {
        const targetRow = clone(target.getRowType());
        const values: any[] = selectRow;
        for (const c of res[0].columns) {
          // @ts-ignore
          targetRow.get()[c]?.set(values.shift());
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