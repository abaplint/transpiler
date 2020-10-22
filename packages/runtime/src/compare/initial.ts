import {ABAPObject} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function initial(val: ICharacter | INumeric | ABAPObject) {
  const value = val.get();
  if (typeof value === "string") {
    return value === "";
  } else if (typeof value === "number") {
    return value === 0;
  } else if (val instanceof ABAPObject) {
    return value === undefined;
  } else {
    throw new Error("runtime, initial, missing implementation");
  }
}