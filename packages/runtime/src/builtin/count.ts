import {initial} from "../compare";
import {Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export type countInput = {
  val: ICharacter,
  sub: ICharacter,
  regex?: ICharacter,
  pcre?: ICharacter,
  case?: ICharacter,
  len?: INumeric,
  off?: INumeric,
};

export function count(input: countInput) {
  let found = 0;

  let val = input.val.get();

  if (input.off) {
    const off = input.off.get();
    val = val.substring(off);
  }
  if (input.len) {
    const len = input.len.get();
    val = val.substring(0, len);
  }

  let reg = "";
  if (input.sub) {
    reg = input.sub.get();
    reg = reg.replace(/\*/g, "\\*");
  } else if (input.regex) {
    reg = input.regex.get();
  } else if (input.pcre) {
    reg = input.pcre.get();
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