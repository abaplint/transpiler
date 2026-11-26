import {FieldSymbol, Numc} from "../types";
import {ICharacter} from "../types/_character";

/** sets the result of a character statement: an n target takes the text as it is,
 * with blanks and other characters, instead of reading it as a number */
export function setText(target: ICharacter, text: string): void {
  const pointer = target instanceof FieldSymbol ? target.getPointer() : target;
  if (pointer instanceof Numc) {
    pointer.set(text, true);
  } else {
    target.set(text);
  }
}
