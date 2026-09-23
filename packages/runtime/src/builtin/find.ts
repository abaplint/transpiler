import {throwError} from "../throw_error";
import {Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {position} from "./_position";

export interface IFindInput {
  val: ICharacter | string;
  sub?: ICharacter | string;
  off?: INumeric | number;
  occ?: INumeric | number;
  len?: INumeric | number;
  regex?: ICharacter | string;
  pcre?: ICharacter | string;
  case?: ICharacter | string;
}

export function find(input: IFindInput) {
  let val = typeof input.val === "string" ? input.val : input.val.get();

  if (input.len !== undefined) {
    throw new Error("transpiler find(), todo len");
  }

  if (input.regex || input.pcre) {
    if (input.off !== undefined) {
      throw new Error("transpiler find(), todo off regex");
    }

    const caseInput = typeof input.case === "string" ? input.case : input.case?.get();
    let regex = "";
    if (input.regex) {
      regex = typeof input.regex === "string" ? input.regex : input.regex.get();
    } else if (input.pcre) {
      regex = typeof input.pcre === "string" ? input.pcre : input.pcre.get();
    }

    const flags = caseInput !== "X" ? "i" : "";
    const reg = new RegExp(regex, flags);

    const ret = val.match(reg)?.index;
    if (ret !== undefined) {
      return new Integer().set(ret);
    } else {
      return new Integer().set(-1);
    }
  } else {
    let sub = typeof input.sub === "string" ? input.sub : input.sub?.get() || "";
    let off = position(input.off) || 0;
    let occ = position(input.occ);

    if (occ === 0) {
      throwError("CX_SY_STRG_PAR_VAL");
    } else if (occ === undefined) {
      occ = 1;
    }

    const caseInput = typeof input.case === "string" ? input.case : input.case?.get();
    if (caseInput !== undefined && caseInput !== "X") {
      val = foldCase(val);
      sub = foldCase(sub);
    }

    if (occ < 0 && sub !== "") {
      // search from the right, on the string as it is, so a "sub" of any length
      // matches and the offset needs no mapping back
      let from = val.length - sub.length;
      let found = -1;
      for (let i = 0; i < Math.abs(occ); i++) {
        found = from < off ? -1 : val.lastIndexOf(sub, from);
        if (found < off) {
          return new Integer().set(-1);
        }
        from = found - 1;
      }
      return new Integer().set(found);
    }

    // only an empty "sub" still comes here with a negative "occ", unchanged
    let negative = false;
    if (occ < 0) {
      negative = true;

      let reversed = "";
      // this is faster than doing val.split("").reverse().join("")
      for (const character of val) {
        reversed = character + reversed;
      }
      val = reversed;

      occ = Math.abs(occ);
    }

    let found = -1;
    for (let i = 0; i < occ; i++) {
      found = val.indexOf(sub, off);
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

// lower case one character at a time and keep a character whose lower case form
// has another length, so every offset stays valid - a plain toLowerCase( )
// turns e.g. "\u0130" into two code units
function foldCase(str: string): string {
  let ret = "";
  for (const character of str) {
    const lower = character.toLowerCase();
    ret += lower.length === character.length ? lower : character;
  }
  return ret;
}
