/*eslint no-bitwise: ["error", { "allow": ["~"] }] */
import {XString} from "../types";

export function bitnot(right: XString) {
  const right16 = parseInt(right.get(),16);
  const not = (~right16).toString(16).toUpperCase();
  const ret = new XString();
  ret.set(not);
  return ret;
}