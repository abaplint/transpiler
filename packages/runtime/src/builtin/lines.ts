import {Table, Integer} from "../types";

export function lines(input: Table): Integer {
  return new Integer({value: input.array().length});
}