import {ICharacter} from "./types/_character";
import {INumeric} from "./types/_numeric";

export function alphaOut(source: ICharacter | INumeric): string {
  const txt: string = source.get() + "";
  return txt.replace(/^0+/, "");
}


export function alphaIn(_source: ICharacter | INumeric, _context: any): string {
  throw new Error("ALPHA_IN_NOT_SUPPORTED, todo");
}