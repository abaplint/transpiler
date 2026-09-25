import {Character, Date, Hex, HexUInt8, Numc, String, Structure, Table, TableKeyType, Time, XString} from "./types";

// the standard key, all character-like and byte-like components, substructures expanded
function standardKeyValues(row: any): any[] {
  if (!(row instanceof Structure)) {
    return [row];
  }
  const ret: any[] = [];
  for (const component of Object.values(row.get())) {
    if (component instanceof Structure) {
      ret.push(...standardKeyValues(component));
    } else if (component instanceof Character
        || component instanceof Numc
        || component instanceof Date
        || component instanceof Time
        || component instanceof String
        || component instanceof Hex
        || component instanceof HexUInt8
        || component instanceof XString) {
      ret.push(component);
    }
  }
  return ret;
}

// values of the primary key, an empty key gives no values
export function primaryKeyValues(target: Table, row: any): any[] {
  const options = target.getOptions();
  if (options?.keyType === TableKeyType.empty) {
    return [];
  }
  const keyFields = options?.primaryKey?.keyFields ?? [];
  if (keyFields.length === 0) {
    return standardKeyValues(row);
  }
  return keyFields.map(k => {
    if (k.toUpperCase() === "TABLE_LINE") {
      return row;
    }
    let value = row;
    for (const name of k.toLowerCase().split("-")) {
      value = value.get()[name];
    }
    return value;
  });
}
