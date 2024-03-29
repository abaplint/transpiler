/* eslint-disable no-bitwise */
import {Hex, HexUInt8, XString} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function getBit(number: INumeric, hex: XString | Hex | HexUInt8, output: ICharacter) {
  const byteIndex = Math.floor((number.get() - 1) / 8);
  const bitIndex = (number.get() - 1) % 8;
  if (hex instanceof HexUInt8) {
    let int = hex.getOffsetRaw(byteIndex);
    int >>= (8 - bitIndex - 1);
    int &= 1;
    // @ts-ignore
    output.set(int);
  } else {
    if (bitIndex < 0) {
      throw new Error("BIT_OFFSET_NOT_POSITIVE");
    }

    const h = hex.get().substr(byteIndex * 2, 2);
    const parsed = parseInt(h, 16).toString(2);
    const bits = parsed.padStart(8, "0");
    output.set(bits.substr(bitIndex, 1));
  }
}