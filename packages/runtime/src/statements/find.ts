import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function find(search: ICharacter | string, input: ICharacter | string, output: INumeric) {
  let s = search;
  if (typeof s !== "string") {
    s = s.get();
  }

  let i = input;
  if (typeof i !== "string") {
    i = i.get();
  }

  const index = i.search(s);
  if (index < 0) {
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(4);
  } else {
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(0);
    output.set(index);
  }

}