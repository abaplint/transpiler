import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {String} from "../types/string";
import {ABAPRegExp} from "../abap_regex";
import {Character, FieldSymbol} from "../types";

export interface IReplaceInput {
  val: string | ICharacter | FieldSymbol,
  sub?: string | ICharacter | FieldSymbol,
  with?: string | ICharacter | FieldSymbol,
  regex?: string | ICharacter | FieldSymbol,
  pcre?: string | ICharacter | FieldSymbol,
  off?: INumeric,
  len?: INumeric,
  occ?: INumeric,
}

function dereference(input: string | ICharacter | FieldSymbol | undefined): string | ICharacter | undefined {
  if (input instanceof FieldSymbol) {
    const pointer = input.getPointer() as ICharacter | undefined;
    if (pointer === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return pointer;
  }
  return input;
}

export function replace(input: IReplaceInput) {
  const source = dereference(input.val)!;
  const withSource = dereference(input.with);
  const subSource = dereference(input.sub);
  const regexInput = dereference(input.pcre || input.regex);

  let val: string | undefined = undefined;
  if (typeof source === "string") {
    val = source;
  } else if (source instanceof Character) {
    val = source.getTrimEnd();
  } else {
    val = source.get();
  }

  let wi: string | undefined = undefined;
  if (typeof withSource === "string") {
    wi = withSource;
  } else if (withSource instanceof Character) {
    wi = withSource.getTrimEnd();
  } else if (withSource) {
    wi = withSource.get();
  }

  let sub: string | RegExp | undefined = undefined;
  if (typeof subSource === "string") {
    sub = subSource;
  } else if (subSource instanceof Character) {
    sub = subSource.getTrimEnd();
  } else if (subSource) {
    sub = subSource.get();
  }
  if (sub !== undefined) {
    sub = ABAPRegExp.escapeRegExp(sub);
  }

  if (typeof regexInput === "string") {
    sub = new RegExp(ABAPRegExp.convert(regexInput), "g");
  } else if (regexInput) {
    sub = new RegExp(ABAPRegExp.convert(regexInput.get()), "g");
  }

  if (input.off && input.len && typeof source === "string") {
    const offset = input.off.get();
    const length = input.len.get();
    val = val.substring(0, offset) + wi + val.substring(offset + length);
  } else if (input.off && input.len && !(typeof source === "string")) {
    const offset = input.off.get();
    const length = input.len.get();
    val = source.getOffset({offset: 0, length: offset}).get() +
          wi +
          source.getOffset({offset: offset + length}).get();
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
