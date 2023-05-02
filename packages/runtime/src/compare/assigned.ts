import {FieldSymbol} from "../types/index.js";

export function assigned(val: FieldSymbol) {
  return val.isAssigned();
}