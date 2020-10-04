import {Hex, XString} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function getBit(number: INumeric, hex: XString | Hex, output: ICharacter) {
  // todo, this is slow
  let bits = parseInt(hex.get(), 16).toString(2);
  bits = bits.padStart(hex.get().length * 2 * 2, "0");

  output.set(bits.substr(number.get() - 1, 1));
}