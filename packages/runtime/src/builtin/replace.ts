import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {String} from "../types/string";

export interface IReplaceInput {
  val: string | ICharacter,
  sub?: string | ICharacter,
  with?: string | ICharacter,
  occ?: INumeric,
}

export function replace(input: IReplaceInput) {
  let val: string | undefined = undefined;
  if (typeof input.val === "string") {
    val = input.val;
  } else {
    val = input.val.get();
  }

  let wi: string | undefined = undefined;
  if (typeof input.with === "string") {
    wi = input.with;
  } else if (input.with) {
    wi = input.with.get();
  }

  let sub: string | undefined = undefined;
  if (typeof input.sub === "string") {
    sub = input.sub;
  } else if (input.sub) {
    sub = input.sub.get();
  }

  if (input.occ === undefined && sub && wi) {
    val = val.replace(sub, wi);
  } else if (input.occ && input.occ.get() === 0 && sub && wi) {
    const reg = new RegExp(sub, "g");
    val = val.replace(reg, wi);
  }

  return new String().set(val);
}