import {Character, FieldSymbol, Structure} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

function escapeRegExpCharacter(input: string): string {
  return input.replace(/[\\^$.*+?()[\]{}|]/g, "\\$&");
}

const CACHE_SIZE = 1000;
const cache = new Map<string, RegExp>();

/** `end` is the pattern's length after the last token that is not a `*`: what
 *  follows is a run of trailing `[\s\S]*`, which matches any rest of the
 *  string - so it goes, and so does the `$` that made the engine walk there */
function compile(r: string): RegExp {
  let pattern = "";
  let end = 0;
  for (let i = 0; i < r.length; i++) {
    const current = r[i];
    if (current === "*") {
      pattern += "[\\s\\S]*";
      continue;
    }
    if (current === "#") {
      if (i + 1 < r.length) {
        const next = r[i + 1];
        pattern += next === "#" ? "#" : escapeRegExpCharacter(next);
        i++;
      } else {
        pattern += "#";
      }
    } else if (current === "+") {
      pattern += "[\\s\\S]";
    } else {
      pattern += escapeRegExpCharacter(current);
    }
    end = pattern.length;
  }
  const open = end < pattern.length;
  return new RegExp("^" + pattern.slice(0, end) + (open ? "" : "$"), "iu");
}

export function cp(left: number | string | ICharacter | INumeric | Structure, right: string | ICharacter): boolean {
  let l = "";
  if (typeof left === "number" || typeof left === "string") {
    l = left.toString();
  } else if (left instanceof Structure) {
    l = left.getCharacter();
  } else if (left instanceof FieldSymbol) {
    if (left.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return cp(left.getPointer(), right);
  } else if (left instanceof Character) {
    l = left.getTrimEnd();
  } else {
    l = left.get().toString();
  }

  let r = "";
  if (typeof right === "string") {
    r = right.toString();
  } else if (right instanceof Character) {
    r = right.getTrimEnd();
  } else {
    r = right.get().toString().trimEnd();
  }

  let reg = cache.get(r);
  if (reg === undefined) {
    reg = compile(r);
    if (cache.size >= CACHE_SIZE) {
      cache.delete(cache.keys().next().value!);
    }
    cache.set(r, reg);
  }
  return reg.test(l);
}