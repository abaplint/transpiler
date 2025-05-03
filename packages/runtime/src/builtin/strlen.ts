import {IntegerFactory} from "../integer_factory";
import {Character, Integer} from "../types";
import {ICharacter} from "../types/_character";

export function strlen(input: {val: ICharacter | string}): Integer {
  let str = "";
  if (typeof input.val === "string") {
    str = input.val;
  } else if (input.val instanceof Character) {
    str = input.val.getTrimEnd();
  } else {
    str = input.val.get();
  }
  if (str.length <= 200) {
    return IntegerFactory.get(str.length);
  } else {
    return new Integer().set(str.length);
  }
}