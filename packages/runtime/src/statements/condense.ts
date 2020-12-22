import {ICharacter} from "../types/_character";

export function condense(input: ICharacter, options: {nogaps: boolean}): void {
  let trimmed = input.get().trim();
  if (options.nogaps) {
    trimmed = trimmed.replace(/\s*/g,"");
  } else {
    trimmed = trimmed.replace(/\s{2,}/g," ");
  }
  input.set(trimmed);
}