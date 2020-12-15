/*eslint no-bitwise: ["error", { "allow": ["&"] }] */
import {XString} from "../types";

export function bitand(left: any, right: any) {
  const left16 = parseInt(left.get(),16);
  const right16 = parseInt(right.get(),16);
  const and = (left16 & right16).toString(16).toUpperCase();
  const ret = new XString();
  ret.set(and);
  return ret;
}