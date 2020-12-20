/* eslint-disable no-bitwise */
import {Hex, XString} from "../types";
import {INumeric} from "../types/_numeric";

export function setBit(number: INumeric, hex: XString | Hex, val?: INumeric) {
  let hexFull = hex.get();
  if (hexFull === "") {
    hexFull = "00";
  }

  const fullByteLength = Math.ceil(hexFull.length / 2);
  hexFull = hexFull.padEnd(fullByteLength * 2, "0");

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
    byte = hexFull.substr((byteNum - 1) * 2,2);
    if (fullByteLength > byteNum) {
      post = hexFull.substr(byteNum * 2,(fullByteLength - byteNum) * 2);
    }
  } else {
    byte = hexFull;
  }

  let bits = parseInt(byte,16);
  const bitMask = 1 << 8 - ( number.get() - ( byteNum - 1 ) * 8 );
  if (val?.get() === 0) {
    bits = bits &= ~bitMask;
  } else {
    bits = bits |= bitMask;
  }
  const reconstructed = pre + bits.toString(16).toUpperCase().padStart(2,"0") + post;
  hex.set(reconstructed);
}