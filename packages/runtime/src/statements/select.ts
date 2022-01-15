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
  if (target instanceof Structure && res[0]?.values[0]) {
    const columns: string[] = res[0].columns;
    const values: any[] = res[0].values[0];
    const result: any = {};
    for (const c of columns) {
      result[c] = clone(target.get()[c]).set(values.shift());
    }
    target.set(new Structure(result));
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(0);
  } else if (target instanceof Structure) {
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(4);
  } else {
    throw new Error("Runtime, SELECT todo");
  }
}