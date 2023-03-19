import {Table, Integer, FieldSymbol} from "../types";

export function lines(input: {val: Table | FieldSymbol}): Integer {
  if (input.val instanceof FieldSymbol
      && input.val.getPointer() === undefined) {
    throw new Error("GETWA_NOT_ASSIGNED");
  }

  return new Integer().set(input.val.array().length);
}