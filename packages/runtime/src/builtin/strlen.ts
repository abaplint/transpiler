import {IntegerFactory} from "../integer_factory";
import {Character, FieldSymbol, Integer} from "../types";
import {ICharacter} from "../types/_character";

export function strlen(input: {val: ICharacter | FieldSymbol | string}): Integer {
  let source: ICharacter | string;
  if (input.val instanceof FieldSymbol) {
    const pointer = input.val.getPointer() as ICharacter | undefined;
    if (pointer === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    source = pointer;
  } else {
    source = input.val;
  }

  let str = "";
  if (typeof source === "string") {
    str = source;
  } else if (source instanceof Character) {
    str = source.getTrimEnd();
  } else {
    str = source.get();
  }
  if (str.length <= 200) {
    return IntegerFactory.get(str.length);
  } else {
    return new Integer().set(str.length);
  }
}
