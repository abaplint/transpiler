/*eslint no-bitwise: ["error", { "allow": ["&"] }] */
import {XString} from "../types/index.js";
import {get_bit_operation_chunks} from "./_bit_operations.js";

export function bitand(left: XString, right: XString) {

  let and = "";
  const chunks = get_bit_operation_chunks(left, right);
  // eslint-disable-next-line no-cond-assign
  for (let i = 0,chunk; chunk = chunks[i]; i++) {
    and = and + (chunk.leftChunk & chunk.rightChunk).toString(16).toUpperCase().padStart(chunk.chunkLen,"0");
  }

  const ret = new XString();
  ret.set(and);
  return ret;
}