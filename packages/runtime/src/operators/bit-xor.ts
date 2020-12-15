/*eslint no-bitwise: ["error", { "allow": ["^"] }] */
import {XString} from "../types";

export function bitxor(left: any, right: any) {
  const left16 = parseInt(left.get(),16);
  const right16 = parseInt(right.get(),16);
  const xor = (left16 ^ right16).toString(16).toUpperCase();
  const ret = new XString();
  ret.set(xor);
  return ret;
}