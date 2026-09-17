import {String} from "../types";
import {throwError} from "../throw_error";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IContainsAnyOfInput {
  val: ICharacter | string,
  sub: ICharacter | string,
  off?: INumeric | number,
  len?: INumeric | number,
}

export function contains_any_of(input: IContainsAnyOfInput) {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  const sub = typeof input.sub === "string" ? input.sub : input.sub.get();
  const off = typeof input.off === "number" ? input.off : input.off?.get() || 0;
  const len = typeof input.len === "number" ? input.len : input.len?.get();

  if (off < 0 || off > val.length) {
    throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
  }
  if (len !== undefined && (len < 0 || off + len > val.length)) {
    throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
  }

  const end = len === undefined ? val.length : off + len;
  const characters = new Set(sub.split(""));

  let found = false;
  for (let i = off; i < end; i++) {
    if (characters.has(val.charAt(i))) {
      found = true;
      break;
    }
  }

  return new String().set(found ? "X" : " ");
}
