import {throwError} from "../throw_error.js";
import {Integer} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";

export interface IFindInput {
  val: ICharacter | string;
  sub?: ICharacter | string;
  off?: INumeric | number;
  occ?: INumeric | number;
  len?: INumeric | number;
  regex?: ICharacter | string;
  case?: ICharacter | string;
}

export function find(input: IFindInput) {
  let val = typeof input.val === "string" ? input.val : input.val.get();

  if (input.len !== undefined) {
    throw "transpiler find(), todo len";
  }

  if (input.regex) {
    if (input.off !== undefined) {
      throw "transpiler find(), todo off regex";
    }

    const caseInput = typeof input.case === "string" ? input.case : input.case?.get();
    const regex = typeof input.regex === "string" ? input.regex : input.regex.get();

    const flags = caseInput !== "X" ? "i" : "";
    const reg = new RegExp(regex, flags);

    const ret = val.match(reg)?.index;
    if (ret !== undefined) {
      return new Integer().set(ret);
    } else {
      return new Integer().set(-1);
    }
  } else {
    const sub = typeof input.sub === "string" ? input.sub : input.sub?.get();
    let off = typeof input.off === "number" ? input.off : input.off?.get() || 0;
    let occ = typeof input.occ === "number" ? input.occ : input.occ?.get();

    if (occ === 0) {
      throwError("CX_SY_STRG_PAR_VAL");
    } else if (occ === undefined) {
      occ = 1;
    }

    let negative = false;
    if (occ < 0) {
      negative = true;
      val = val.split("").reverse().join("");
      occ = Math.abs(occ);
    }

    let found = -1;
    for (let i = 0; i < occ; i++) {
      found = val.indexOf(sub || "", off);
      if (found >= 0) {
        off = found + 1;
      }
    }

    if (negative === true && found >= 0) {
      found = val.length - found - 1;
    }

    return new Integer().set(found);
  }
}