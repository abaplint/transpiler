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
  let foundIndex = 0;

  const arr = table.array();

  if (options?.index) {
    let index = options.index;
    if (typeof index !== "number") {
      index = index.get();
    }

    found = arr[index - 1];
    if (found) {
      foundIndex = index;
    }
  } else if (options?.withKey) {
    const isStructured = arr[0] instanceof Structure;
    for (const a of arr) {
      foundIndex++;
      const row = isStructured ? a.get() : {table_line: a};
      if (options.withKey(row) === true) {
        found = a;
        break;
      }
    }
    if (found === undefined) {
      foundIndex = 0;
    }

  } else {
    throw new Error("runtime, readTable, unexpected input");
  }

  const subrc = found ? 0 : 4;
  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);
  // @ts-ignore
  abap.builtin.sy.get().tabix.set(foundIndex);

  if (options.into && found) {
    if (options.into instanceof DataReference && found instanceof DataReference) {
      options.into.assign(found.getPointer());
    } else if (options.into instanceof DataReference) {
      options.into.assign(found);
    } else {
      options.into.set(found);
    }
  } else if (options.assigning && found) {
    options.assigning.assign(found);
  }

}