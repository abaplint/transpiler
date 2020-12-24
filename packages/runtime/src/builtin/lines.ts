import {Table, Integer} from "../types";

export function lines(input: {val: Table}): Integer {
  return new Integer().set(input.val.array().length);
}