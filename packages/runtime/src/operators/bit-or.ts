/*eslint no-bitwise: ["error", { "allow": ["|"] }] */
import {XString} from "../types/index.js";
import {get_bit_operation_chunks} from "./_bit_operations.js";

export function bitor(left: XString, right: XString) {
  let or = "";
  const chunks = get_bit_operation_chunks(left, right);
  // eslint-disable-next-line no-cond-assign
  for (let i = 0,chunk; chunk = chunks[i]; i++) {
    or = or + (chunk.leftChunk | chunk.rightChunk).toString(16).toUpperCase().padStart(chunk.chunkLen,"0");
  }

  const ret = new XString();
  ret.set(or);
  return ret;
}

