import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function condense(input: {
  val: ICharacter | INumeric | string,
  from?: ICharacter | string,
  to?: ICharacter | string,
  del?: ICharacter | string}) {

  let str = typeof input.val === "string" ? input.val : input.val.get().toString();
  let from = " ";
  if (input.from) {
    from = typeof input.from === "string" ? input.from : input.from.get().toString();
  }
  let to = " ";
  if (input.to) {
    to = typeof input.to === "string" ? input.to : input.to.get().toString();
  }
  let del = " ";
  if (input.del) {
    del = typeof input.del === "string" ? input.del : input.del.get().toString();
  }

  const beginning = new RegExp(`[${del}]+$`);
  const end = new RegExp(`^[${del}]+`);
  str = str.replace(beginning, "");
  str = str.replace(end, "");

  for (const f of from.split("")) {
    str = str.replace(new RegExp(f.replace(".", "\\."), "g"), to);
  }

  return new String().set(str.replace(/ {2,}/g, " "));
}