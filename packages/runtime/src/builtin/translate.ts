import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function translate(input: {val: ICharacter | string, from: ICharacter | string, to: ICharacter | string}): ICharacter {
  let val = typeof input.val === "string" ? input.val : input.val.get();
  const from = typeof input.from === "string" ? input.from : input.from.get();
  const to = typeof input.to === "string" ? input.to : input.to.get();

  const fromSplit = from.split("");
  const toSplit = to.split("");

  for (let i = 0; i < fromSplit.length; i++) {
    const reg = new RegExp(fromSplit[i], "g");
    val = val.replace(reg, toSplit[i] || "");
  }

  return new String().set(val);
}