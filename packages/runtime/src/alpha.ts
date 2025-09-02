import {Character, String} from "./types";
import {ICharacter} from "./types/_character";
import {INumeric} from "./types/_numeric";

export function alphaOut(source: ICharacter | INumeric): string {
  const txt: string = source.get() + "";
  return txt.replace(/^0+/, "");
}

export function alphaIn(source: ICharacter | INumeric | String, context: Character | String | undefined): string {
  let txt: string = source.get() + "";

  if (context === undefined) {
    throw new Error("alphaIn, context is undefined");
  }

  if (txt.match(/^[0-9 ]+$/) === null) {
    // contains letters
    return txt;
  }

  let length = 0;
  if (context instanceof String && source instanceof String) {
    return txt;
  } else if (context instanceof Character) {
    length = context.getLength();
  } else if (source instanceof Character) {
    length = source.getLength();
  }
  txt = txt.trimEnd();

  return txt.padStart(length, "0");
}