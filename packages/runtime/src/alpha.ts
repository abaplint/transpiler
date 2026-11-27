import {Character, String} from "./types";
import {ICharacter} from "./types/_character";
import {INumeric} from "./types/_numeric";

export function alphaOut(source: ICharacter | INumeric): string {
  const txt: string = source.get() + "";
  return txt.replace(/^0+/, "");
}

/** ALPHA = OUT in a string template: a text of digits loses its leading blanks and zeros,
 * all zeros give "0", any other text is kept; the result keeps the length of the source,
 * padded with blanks on the right */
export function alphaOutTemplate(text: string, length: number): string {
  if (text.match(/^ *\d+$/) !== null) {
    text = text.replace(/^[ 0]+/, "");
    if (text === "") {
      text = "0";
    }
  }
  return text.padEnd(length, " ");
}

export function alphaIn(source: ICharacter | INumeric | String, context: Character | String | undefined): string {
  let txt: string = source.get() + "";

  if (txt.trim() === "") {
    // initial value stays initial
    return "";
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