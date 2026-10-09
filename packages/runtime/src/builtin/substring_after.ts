import {ICharacter} from "../types/_character";
import {String} from "../types/string";
import {ISubstringSearchInput, substringLength, substringOccurrence} from "./_substring_occurrence";

/** The text after the occurrence occ of sub or regex, at most len characters of it */
export function substring_after(input: ISubstringSearchInput): ICharacter {
  const found = substringOccurrence(input);
  if (found === undefined) {
    return new String().set("");
  }
  const start = found.offset + found.length;
  const len = substringLength(input.len, found.val.length - start);
  return new String().set(found.val.substring(start, len === undefined ? undefined : start + len));
}
