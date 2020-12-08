import {INumeric} from "../types/_numeric";
import {ICharacter} from "../types/_character";
import {FieldSymbol, Structure, Table} from "../types";

export function loop(
  table: Table,
  into: ICharacter | INumeric | Structure | FieldSymbol,
  where: (i: any) => boolean | undefined,
  callback: () => void) {

  const array = table.array();
  for (let i = 0; i < array.length; i++) {
    // @ts-ignore
    abap.builtin.sy.get().tabix.set(i + 1);

    if (where) {
      const row = array[i] instanceof Structure ? array[i].get() : {table_line: array[i]};
      if (where(row) === false) {
        continue;
      }
    }

    if (into instanceof FieldSymbol) {
      into.assign(array[i]);
    } else if (into?.set) {
      into.set(array[i]);
    } else {
      into = array[i]; // its a field symbol
    }
    callback();
  }
}