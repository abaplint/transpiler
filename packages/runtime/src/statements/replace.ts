import {ICharacter} from "../types/_character";

export function replace(input: ICharacter, all: boolean, s: ICharacter | string, r: ICharacter | string): void {
  let temp = input.get();

  let search: string = "";
  if (typeof s === "string") {
    search = s;
  } else {
    search = s.get();
  }

  let replace: string = "";
  if (typeof r === "string") {
    replace = r;
  } else {
    replace = r.get();
  }

  const found = temp.search(search) >= 0;

  if (all === true) {
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

  input.set(temp);
}