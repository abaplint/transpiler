import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {String} from "../types/string";
import {ABAPRegExp} from "../abap_regex";
import {Character} from "../types";

export interface IReplaceInput {
  val: string | ICharacter,
  sub?: string | ICharacter,
  with?: string | ICharacter,
  regex?: string | ICharacter,
  off?: INumeric,
  len?: INumeric,
  occ?: INumeric,
}

export function replace(input: IReplaceInput) {
  let val: string | undefined = undefined;
  if (typeof input.val === "string") {
    val = input.val;
  } else if (input.val instanceof Character) {
    val = input.val.getTrimEnd();
  } else {
    val = input.val.get();
  }

  let wi: string | undefined = undefined;
  if (typeof input.with === "string") {
    wi = input.with;
  } else if (input.with instanceof Character) {
    wi = input.with.getTrimEnd();
  } else if (input.with) {
    wi = input.with.get();
  }

  let sub: string | RegExp | undefined = undefined;
  if (typeof input.sub === "string") {
    sub = input.sub;
  } else if (input.sub instanceof Character) {
    sub = input.sub.getTrimEnd();
  } else if (input.sub) {
    sub = input.sub.get();
  }
  if (sub !== undefined) {
    sub = ABAPRegExp.escapeRegExp(sub);
  }

  if (typeof input.regex === "string") {
    sub = new RegExp(ABAPRegExp.convert(input.regex), "g");
  } else if (input.regex) {
    sub = new RegExp(ABAPRegExp.convert(input.regex.get()), "g");
  }

  if (input.off && input.len && typeof input.val === "string") {
    const offset = input.off.get();
    const length = input.len.get();
    val = val.substring(0, offset) + wi + val.substring(offset + length);
  } else if (input.off && input.len && !(typeof input.val === "string")) {
    const offset = input.off.get();
    const length = input.len.get();
    val = input.val.getOffset({offset: 0, length: offset}).get() +
          wi +
          input.val.getOffset({offset: offset + length}).get();
  } else if (input.occ === undefined && sub && wi !== undefined) {
    if (typeof sub === "string") {
      sub = new RegExp(sub);
    }
    val = val.replace(sub, wi);
  } else if (input.occ && input.occ.get() === 0 && sub && wi !== undefined) {
    if (typeof sub === "string") {
      sub = new RegExp(sub, "g");
    }
    val = val.replace(sub, wi);
  }

  return new String().set(val);
}