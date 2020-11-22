import {ABAPObject, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function initial(val: ICharacter | INumeric | Structure | Table | ABAPObject) {
  if (val instanceof Table) {
    return val.array().length === 0;
  }
  const value = val.get();
  if (typeof value === "string") {
    return value === "";
  } else if (typeof value === "number") {
    return value === 0;
  } else if (val instanceof ABAPObject) {
    return value === undefined;
  } else if (typeof value === "object") {
    for (const f of Object.keys(value)) {
      if (initial(value[f]) === false) {
        return false;
      }
    }
    return true;
  } else {
    throw new Error("runtime, initial, missing implementation");
  }
}