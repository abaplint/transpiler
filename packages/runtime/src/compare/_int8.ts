import {parse} from "../operators/_parse";
import {Hex, HexUInt8, Integer8} from "../types";

/** an operand compared with an int8, as a bigint: an x field is converted to
 * int8 like in a move (the last eight bytes, signed), not through a number,
 * which would lose precision above 2^53 */
export function int8Operand(value: any): bigint {
  if (value instanceof Integer8) {
    return value.get();
  } else if (value instanceof Hex || value instanceof HexUInt8) {
    return new Integer8().set(value).get();
  }
  return BigInt(parse(value));
}
