import {Table, Integer, FieldSymbol} from "../types";

export function lines(input: {val: Table | FieldSymbol}): Integer {
  if (input.val instanceof FieldSymbol) {
    if (input.val.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return lines({val: input.val.getPointer()});
  }

  return new Integer().set(input.val.getArrayLength());
}