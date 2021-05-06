import {ICharacter} from "../types/_character";

export function replace(input: ICharacter, s: ICharacter | string, r: ICharacter | string): void {
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

  if (search.length === 0) {
    throw "REPLACE, zero length input";
  }

  while(temp.replace(search, replace) !== temp) {
    temp = temp.replace(search, replace);
  }

  input.set(temp);
}