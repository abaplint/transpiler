import {initial} from "../compare";
import {throwError} from "../throw_error";
import {Character, FieldSymbol} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {position} from "./_position";
import {find} from "./find";

export interface ISubstringSearchInput {
  val: ICharacter | FieldSymbol | string,
  sub?: ICharacter | string,
  regex?: ICharacter | string,
  pcre?: ICharacter | string,
  case?: ICharacter | string,
  occ?: INumeric | number,
  len?: INumeric | number,
}

/** The text and the occurrence substring_after( ), _before( ), _from( ) and _to( ) cut at,
 *  undefined when there is none. An empty sub or regex and occ = 0 raise CX_SY_STRG_PAR_VAL. */
export function substringOccurrence(input: ISubstringSearchInput): {val: string, offset: number, length: number} | undefined {
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
  } else if (source instanceof Character) {
    val = source.getTrimEnd();
  } else {
    val = source.get();
  }

  const text = (v: ICharacter | string | undefined) => typeof v === "string" ? v : v?.get();
  const regex = text(input.regex) ?? text(input.pcre);
  const sub = text(input.sub);

  if (regex === undefined) {
    if (sub === undefined || sub === "") {
      throwError("CX_SY_STRG_PAR_VAL");
    }
    // the same occurrence find( ) answers, from the left or the right, with or without case
    const offset = find({val, sub, occ: input.occ, case: input.case}).get();
    return offset < 0 ? undefined : {val, offset, length: sub!.length};
  }

  if (regex === "") {
    throwError("CX_SY_STRG_PAR_VAL");
  }
  let occ = position(input.occ) ?? 1;
  if (occ === 0) {
    throwError("CX_SY_STRG_PAR_VAL");
  }

  const insensitive = input.case !== undefined && initial(input.case);
  const matches = [...val.matchAll(new RegExp(regex, insensitive ? "gi" : "g"))];
  if (occ < 0) {
    occ = matches.length + occ + 1;
  }
  const match = matches[occ - 1];
  return match === undefined ? undefined : {val, offset: match.index!, length: match[0].length};
}

/** len as substring_after( ) and its kin read it: a negative one, or one past the text, is out of bounds */
export function substringLength(len: INumeric | number | undefined, available: number): number | undefined {
  const ret = position(len);
  if (ret !== undefined && (ret < 0 || ret > available)) {
    throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
  }
  return ret;
}
