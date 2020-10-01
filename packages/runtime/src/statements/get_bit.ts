import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function getBit(number: INumeric, hex: ICharacter, output: ICharacter) {
  // todo, this is slow
  const bits = parseInt(hex.get(), 16).toString(2);

  output.set(bits.substr(number.get() - 1, 1));
}