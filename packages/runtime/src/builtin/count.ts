import {Integer} from "../types";
import {ICharacter} from "../types/_character";

export function count(input: {val: ICharacter | string, sub: ICharacter | string}) {
  let found = 0;

  const val = typeof input.val === "string" ? input.val : input.val.get();
  const sub = typeof input.sub === "string" ? input.sub : input.sub.get();

  if (val !== "") {
    const res = val.match(new RegExp(sub, "g"));
    if (res) {
      found = res.length;
    }
  }


  return new Integer().set(found);
}