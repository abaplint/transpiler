/*eslint no-bitwise: ["error", { "allow": ["^"] }] */
import {XString} from "../types";
import {bit_operation} from "./_bit_operations";

export function bitxor(left: XString, right: XString) {
  return bit_operation(left, right, (l, r) => l ^ r);
}
