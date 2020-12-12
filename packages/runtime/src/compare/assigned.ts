import {FieldSymbol} from "../types";

export function assigned(val: FieldSymbol) {
  return val.isAssigned();
}