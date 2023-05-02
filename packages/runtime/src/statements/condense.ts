import {ICharacter} from "../types/_character.js";

export function condense(input: ICharacter, options: {nogaps: boolean}): void {
  let trimmed = input.get().replace(/ +$/, "");
  trimmed = trimmed.replace(/^ +/, "");
  if (options.nogaps) {
    trimmed = trimmed.replace(/ */g,"");
  } else {
    trimmed = trimmed.replace(/ {2,}/g," ");
  }
  input.set(trimmed);
}