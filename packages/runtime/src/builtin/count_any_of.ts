import {Integer} from "../types";
import {ICharacter} from "../types/_character";

type countAnyOfInput = {
  val: ICharacter,
  sub: ICharacter,
};

export function count_any_of(input: countAnyOfInput) {
  let found = 0;

  const val = input.val.get();
  const sub = input.sub.get();

  if (sub !== "") {
    for (const char of sub.split("")) {
      const match = val.match(new RegExp(char, "g"));
      found += match?.length || 0;
    }
  }

  return new Integer().set(found);
}