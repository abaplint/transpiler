import {ICharacter} from "./types/_character";
import {INumeric} from "./types/_numeric";

export function alphaOut(source: ICharacter | INumeric) {
  const txt: string = source.get() + "";
  return txt.replace(/^0+/, "");
}


export function alphaIn() {
  throw new Error("ALPHA_IN_NOT_SUPPORTED, todo");
}