import {initial} from "../compare";
import {Integer} from "../types";
import {ICharacter} from "../types/_character";

export type countInput = {
  val: ICharacter,
  sub: ICharacter,
  regex?: ICharacter,
  case?: ICharacter,
};

export function count(input: countInput) {
  let found = 0;

  const val = input.val.get();

  let reg = "";
  if (input.sub) {
    reg = input.sub.get();
    reg = reg.replace(/\*/g, "\\*");
  } else if (input.regex) {
    reg = input.regex.get();
  }

  let options = "g";

  if (input.case && initial(input.case)) {
    options += "i";
  }

  if (val !== "") {
    const res = val.match(new RegExp(reg, options));
    if (res) {
      found = res.length;
    }
  }

  return new Integer().set(found);
}