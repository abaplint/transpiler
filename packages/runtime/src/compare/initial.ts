import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function initial(val: ICharacter | INumeric) {
  const value = val.get();
  if (typeof value === "string") {
    return value === "";
  } else if (typeof value === "number") {
    return value === 0;
  }
  return false;
}