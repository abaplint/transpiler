import {ICharacter} from "../types/_character";

export type replaceInput = {
  target: ICharacter,
  sectionLength?: ICharacter,
  regex?: ICharacter,
  all: boolean,
  with: ICharacter,
  of: ICharacter,
};

export function replace(input: replaceInput): void {
  let temp = input.target.get();

  let search: string = "";
  if (input.of) {
    search = input.of.get();
  } else if (input.regex) {
    search = input.regex.get();
  }

  let replace: string = "";
  if (typeof input.with === "string") {
    replace = input.with;
  } else {
    replace = input.with.get();
  }

  const found = temp.indexOf(search) >= 0;

  if (input.all === true) {
    if (search.length === 0) {
      throw "REPLACE, zero length input";
    }
    while(temp.replace(search, replace) !== temp) {
      temp = temp.replace(search, replace);
    }
  } else {
    temp = temp.replace(search, replace);
  }

  const subrc = found ? 0 : 4;
  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);

  input.target.set(temp);
}