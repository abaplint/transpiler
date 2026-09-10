import {Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {throwError} from "../throw_error";

export interface IFindAnyNotOfInput {
  val: ICharacter | string;
  sub: ICharacter | string;
  off?: INumeric | number;
  len?: INumeric | number;
  occ?: INumeric | number;
}

export function find_any_not_of(input: IFindAnyNotOfInput) {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  const sub = typeof input.sub === "string" ? input.sub : input.sub.get();

  const off = typeof input.off === "number" ? input.off : input.off?.get() || 0;
  const len = typeof input.len === "number" ? input.len : input.len?.get();
  let occ = typeof input.occ === "number" ? input.occ : input.occ?.get();

  if (occ === 0) {
    throwError("CX_SY_STRG_PAR_VAL");
  } else if (occ === undefined) {
    occ = 1;
  }

  if (off < 0 || off > val.length) {
    throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
  }
  if (len !== undefined && (len < 0 || off + len > val.length)) {
    throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
  }

  const end = len === undefined ? val.length : off + len;
  const characters = new Set(sub.split(""));

  let remaining = Math.abs(occ);
  if (occ > 0) {
    for (let i = off; i < end; i++) {
      if (!characters.has(val.charAt(i)) && --remaining === 0) {
        return new Integer().set(i);
      }
    }
  } else {
    for (let i = end - 1; i >= off; i--) {
      if (!characters.has(val.charAt(i)) && --remaining === 0) {
        return new Integer().set(i);
      }
    }
  }

  return new Integer().set(-1);
}
