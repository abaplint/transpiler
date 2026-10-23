import {Character, FieldSymbol} from "../types";
import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function translate(input: {val: ICharacter | string, from: ICharacter | string, to: ICharacter | FieldSymbol | string}): ICharacter {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  const from = typeof input.from === "string" ? input.from : input.from.get();

  let toSource: ICharacter | string;
  if (input.to instanceof FieldSymbol) {
    const pointer = input.to.getPointer() as ICharacter | undefined;
    if (pointer === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    toSource = pointer;
  } else {
    toSource = input.to;
  }

  let to = typeof toSource === "string" ? toSource : toSource.get();
  if (toSource instanceof Character) {
    to = toSource.getTrimEnd();
  }

  // FROM is a list of characters, never a pattern: the character at position n of
  // FROM becomes the character at position n of TO, and is removed when TO has no
  // character there; the first occurrence of a repeated FROM character counts
  const map = new Map<string, string>();
  for (let i = 0; i < from.length; i++) {
    if (map.has(from[i]) === false) {
      map.set(from[i], to[i] ?? "");
    }
  }

  let result = "";
  for (const c of val) {
    const mapped = map.get(c);
    result += mapped === undefined ? c : mapped;
  }

  return new String().set(result);
}
