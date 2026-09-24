/*eslint no-bitwise: ["error", { "allow": [">>>"] }] */
import {XString} from "../types";

// the shorter operand is padded with hex 0 on the right
export function bit_operation(left: XString, right: XString, op: (l: number, r: number) => number): XString {
  let leftHex = left.get();
  let rightHex = right.get();
  const maxLen = Math.ceil(Math.max(leftHex.length, rightHex.length) / 2) * 2;
  leftHex = leftHex.padEnd(maxLen, "0");
  rightHex = rightHex.padEnd(maxLen, "0");

  let result: string;
  if (maxLen > 0 && maxLen <= 8) {
    // up to 4 bytes in one 32 bit operation, >>> 0 makes the result unsigned
    result = (op(parseInt(leftHex, 16), parseInt(rightHex, 16)) >>> 0).toString(16).padStart(maxLen, "0");
  } else {
    const l = Buffer.from(leftHex, "hex");
    const r = Buffer.from(rightHex, "hex");
    for (let i = 0; i < l.length; i++) {
      l[i] = op(l[i], r[i]);
    }
    result = l.toString("hex");
  }

  const ret = new XString();
  ret.set(result.toUpperCase());
  return ret;
}
