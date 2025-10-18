import {Character, FieldSymbol, Structure} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

function escapeRegExpCharacter(input: string): string {
  return input.replace(/[\\^$.*+?()[\]{}|]/g, "\\$&");
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

  let pattern = "";
  for (let i = 0; i < r.length; i++) {
    const current = r[i];
    if (current === "#") {
      if (i + 1 < r.length) {
        const next = r[i + 1];
        if (next === "#") {
          pattern += "#";
          i++;
        } else {
          pattern += escapeRegExpCharacter(next);
          i++;
        }
      } else {
        pattern += "#";
      }
    } else if (current === "*") {
      pattern += "[\\s\\S]*";
    } else if (current === "+") {
      pattern += "[\\s\\S]";
    } else {
      pattern += escapeRegExpCharacter(current);
    }
  }

  const reg = new RegExp("^" + pattern + "$", "iu");

  return l.match(reg) !== null;
}