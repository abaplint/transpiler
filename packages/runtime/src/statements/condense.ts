import {ICharacter} from "../types/_character";

const ENDS_WITH_SPACE = / +$/;
const BEGINS_WITH_SPACE = /^ +/;
const ANY_SPACES = / */g;
const MULTIPLE_SPACES_REGEX = / {2,}/g;

export function condense(input: ICharacter, options: {nogaps: boolean}): void {
  let trimmed = input.get().replace(ENDS_WITH_SPACE, "");
  trimmed = trimmed.replace(BEGINS_WITH_SPACE, "");
  if (options.nogaps) {
    trimmed = trimmed.replace(ANY_SPACES, "");
  } else {
    trimmed = trimmed.replace(MULTIPLE_SPACES_REGEX, " ");
  }
  input.set(trimmed);
}