import {Character, FieldSymbol, String} from "../types";
import {ICharacter} from "../types/_character";

export function to_lower(input: {val: ICharacter | FieldSymbol | string}): ICharacter {
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

  const val = typeof source === "string" ? source : source.get();

  if (source instanceof Character) {
    return new Character(source.getLength()).set(val.toLowerCase());
  } else {
    return new String().set(val.toLowerCase());
  }
}
