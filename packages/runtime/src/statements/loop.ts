import {INumeric} from "../types/_numeric";
import {ICharacter} from "../types/_character";
import {FieldSymbol, Structure, Table} from "../types";

export function loop(table: Table, into: ICharacter | INumeric | Structure | FieldSymbol, callback: () => void) {
  const array = table.array();
  for (let i = 0; i < array.length; i++) {
    // @ts-ignore
    abap.builtin.sy.get().tabix.set(i + 1);
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