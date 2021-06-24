/*eslint no-bitwise: ["error", { "allow": ["~"] }] */
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_NOT
import {Hex, XString} from "../types";

export function bitnot(right: XString | Hex) {
  const right16 = parseInt(right.get(), 16);
  const not = ~right16;
  const ret = new Hex({length: right.get().length / 2});
  ret.set(not);
  return ret;
}