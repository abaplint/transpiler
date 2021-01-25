import {DataReference, FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IReadTableOptions {
  index?: INumeric | number,
  withKey?: (i: any) => boolean,
  into?: INumeric | ICharacter | Structure | Table | DataReference,
  assigning?: FieldSymbol,
}

export function readTable(table: Table | FieldSymbol, options?: IReadTableOptions) {
  let found: any = undefined;

  const arr = table.array();

  if (options?.index) {
    let index = options.index;
    if (typeof index !== "number") {
      index = index.get();
    }

    found = arr[index - 1];
  } else if (options?.withKey) {
    const isStructured = arr[0] instanceof Structure;
    for (const a of arr) {
      const row = isStructured ? a.get() : {table_line: a};
      if (options.withKey(row) === true) {
        found = a;
        break;
      }
    }

  } else {
    throw new Error("runtime, readTable, unexpected input");
  }

  const subrc = found ? 0 : 4;
  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);

  if (options.into && found) {
    if (options.into instanceof DataReference) {
      options.into.assign(found);
    } else {
      options.into.set(found);
    }
  } else if (options.assigning && found) {
    options.assigning.assign(found);
  }

}