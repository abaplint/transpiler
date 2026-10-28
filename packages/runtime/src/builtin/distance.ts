import {Character, FieldSymbol, Integer} from "../types";
import {ICharacter} from "../types/_character";

export interface IDistanceInput {
  val1: ICharacter | FieldSymbol | string;
  val2: ICharacter | FieldSymbol | string;
}

/** Reads a character-like argument; trailing blanks of a text field (type c) are ignored. */
function text(input: ICharacter | FieldSymbol | string): string {
  let source: ICharacter | string;
  if (input instanceof FieldSymbol) {
    const pointer = input.getPointer() as ICharacter | undefined;
    if (pointer === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    source = pointer;
  } else {
    source = input;
  }

  if (typeof source === "string") {
    return source;
  } else if (source instanceof Character) {
    return source.getTrimEnd();
  } else {
    return source.get();
  }
}

/** Levenshtein distance: the fewest characters to insert, delete or replace to turn val1
 * into val2. Characters are UTF-16 code units, as in ABAP. */
export function distance(input: IDistanceInput): Integer {
  const val1 = text(input.val1);
  const val2 = text(input.val2);

  if (val1 === val2) {
    return new Integer().set(0);
  }

  let previous: number[] = [];
  for (let j = 0; j <= val2.length; j++) {
    previous.push(j);
  }

  for (let i = 1; i <= val1.length; i++) {
    const current: number[] = [i];
    for (let j = 1; j <= val2.length; j++) {
      const replace = previous[j - 1] + (val1.charAt(i - 1) === val2.charAt(j - 1) ? 0 : 1);
      current.push(Math.min(previous[j] + 1, current[j - 1] + 1, replace));
    }
    previous = current;
  }

  return new Integer().set(previous[val2.length]);
}
