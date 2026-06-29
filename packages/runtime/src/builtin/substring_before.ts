import {ABAPRegExp} from "../abap_regex";
import {Character, FieldSymbol} from "../types";
import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function substring_before(input: {
  val: ICharacter | FieldSymbol | string,
  sub?: ICharacter | string,
  regex?: ICharacter | string}): ICharacter {
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

  let val = "";
  if (typeof source === "string") {
    val = source;
  } else if (source instanceof Character){
    val = source.getTrimEnd();
  } else {
    val = source.get();
  }

  let reg = "";
  if (typeof input.regex === "string") {
    reg = input.regex;
  } else if (input?.regex) {
    reg = input.regex.get();
  } else if (typeof input.sub === "string") {
    reg = ABAPRegExp.escapeRegExp(input.sub);
  } else if (input?.sub) {
    reg = ABAPRegExp.escapeRegExp(input.sub.get());
  }

  const r = new RegExp("(.*?)" + reg);
  const res = val.match(r);

  let ret = "";
  if (res && res[1]) {
    ret = res[1];
  }
  return new String().set(ret);
}
