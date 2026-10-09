import {ICharacter} from "../types/_character";
import {String} from "../types/string";
import {ISubstringSearchInput, substringLength, substringOccurrence} from "./_substring_occurrence";

/** The text before the occurrence occ of sub or regex, the len characters right in front of it */
export function substring_before(input: ISubstringSearchInput): ICharacter {
  const found = substringOccurrence(input);
  if (found === undefined) {
    return new String().set("");
  }
  const len = substringLength(input.len, found.offset);
  return new String().set(found.val.substring(len === undefined ? 0 : found.offset - len, found.offset));
}
