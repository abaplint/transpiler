/* eslint-disable no-bitwise */
import {Hex, HexUInt8, XString} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function setBit(number: INumeric, hex: XString | Hex | HexUInt8, val?: INumeric | ICharacter) {
  if (number.get() <= 0) {
    throw new Error("BIT_OFFSET_NOT_POSITIVE");
  } else if (hex instanceof HexUInt8) {
    const byteIndex = Math.floor((number.get() - 1) / 8);
    const bitIndex = (number.get() - 1) % 8;

    let bits = hex.getOffsetRaw(byteIndex);
    const bitMask = 1 << ( 8 - bitIndex - 1 );
    if (val?.get() === 0 || val?.get() === "0") {
      bits &= ~bitMask;
    } else {
      bits |= bitMask;
    }
    hex.setOffset(byteIndex, bits);
  } else {
    const hexFull = hex.get();
    const fullByteLength = Math.ceil(hexFull.length / 2);
    const byteNum = Math.ceil(number.get() / 8);

    if (byteNum > fullByteLength) {
      return;
    }

    let pre = "";
    let byte = "";
    let post = "";
    if (hexFull.length > 2) {
      if (byteNum > 1) {
        pre = hexFull.substr(0,(byteNum - 1) * 2);
      }
      byte = hexFull.substr((byteNum - 1) * 2, 2);
      if (fullByteLength > byteNum) {
        post = hexFull.substr(byteNum * 2, (fullByteLength - byteNum) * 2);
      }
    } else {
      byte = hexFull;
    }

    let bits = parseInt(byte,16);
    const bitMask = 1 << 8 - ( number.get() - ( byteNum - 1 ) * 8 );
    if (val?.get() === 0 || val?.get() === "0") {
      bits &= ~bitMask;
    } else {
      bits |= bitMask;
    }

    const reconstructed = pre + bits.toString(16).toUpperCase().padStart(2, "0") + post;
    hex.set(reconstructed);
  }
}