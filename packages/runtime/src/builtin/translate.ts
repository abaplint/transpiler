import {Character, FieldSymbol} from "../types";
import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function translate(input: {val: ICharacter | string, from: ICharacter | string, to: ICharacter | FieldSymbol | string}): ICharacter {
  let val = typeof input.val === "string" ? input.val : input.val.get();
  const from = typeof input.from === "string" ? input.from : input.from.get();

  let toSource: ICharacter | string;
  if (input.to instanceof FieldSymbol) {
    const pointer = input.to.getPointer() as ICharacter | undefined;
    if (pointer === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    toSource = pointer;
  } else {
    toSource = input.to;
  }

  let to = typeof toSource === "string" ? toSource : toSource.get();
  if (toSource instanceof Character) {
    to = toSource.getTrimEnd();
  }

  const fromSplit = from.split("");
  const toSplit = to.split("");

  const chars: {[index: string]: string} = {};
  for (let i = 0; i < fromSplit.length; i++) {
    chars[fromSplit[i]] = toSplit[i] || "";
  }

  const reg = new RegExp("[" + from + "]", "g");
  val = val.replace(reg, m => chars[m] || "");

  return new String().set(val);
}
