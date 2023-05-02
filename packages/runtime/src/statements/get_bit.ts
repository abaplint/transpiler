import {Hex, XString} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";

export function getBit(number: INumeric, hex: XString | Hex, output: ICharacter) {

  const charIndex = Math.floor((number.get() - 1) / 8);
  const bitIndex = (number.get() - 1) % 8;

  const h = hex.get().substr(charIndex * 2, 2);
  const parsed = parseInt(h, 16).toString(2);
  const bits = parsed.padStart(8, "0");

  output.set(bits.substr(bitIndex, 1));
}