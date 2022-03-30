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

  let search: RegExp | string = "";
  let found = false;
  if (input.of) {
    search = input.of.get();
    if (search.length === 0 && input.all === true) {
      throw "REPLACE, zero length input";
    }
    found = temp.indexOf(search) >= 0;
  } else if (input.regex) {
    if (input.regex.get().length === 0 && input.all === true) {
      throw "REPLACE, zero length input";
    }
    found = temp.match(search) !== null;
    search = new RegExp(input.regex.get());
  }

  let replace: string = "";
  if (typeof input.with === "string") {
    replace = input.with;
  } else {
    replace = input.with.get();
  }

  if (input.all === true) {
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