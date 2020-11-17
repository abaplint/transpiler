import {String} from "../types";

export function boolc(input: boolean) {
  const val = input === true ? "X" : "";
  return new String().set(val);
}