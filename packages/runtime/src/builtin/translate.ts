import {Character} from "../types";
import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function translate(input: {val: ICharacter | string, from: ICharacter | string, to: ICharacter | string}): ICharacter {
  let val = typeof input.val === "string" ? input.val : input.val.get();
  const from = typeof input.from === "string" ? input.from : input.from.get();

  let to = typeof input.to === "string" ? input.to : input.to.get();
  if (input.to instanceof Character) {
    to = input.to.getTrimEnd();
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