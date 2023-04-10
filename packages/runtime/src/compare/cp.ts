import {FieldSymbol, Structure} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

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
  } else {
    l = left.get().toString();
  }

  let r = "";
  if (typeof right === "string") {
    r = right.toString();
  } else {
    r = right.get().toString().trimEnd();
  }

  r = r.replace(/\\/g, "\\\\");
  r = r.replace(/\[/g, "\\[");
  r = r.replace(/\]/g, "\\]");
  r = r.replace(/\}/g, "\\}");
  r = r.replace(/\{/g, "\\{");
  r = r.replace(/\?/g, "\\?");
  r = r.replace(/\(/g, "\\(");
  r = r.replace(/\)/g, "\\)");
  r = r.replace(/\./g, "\\.");
  r = r.replace(/\|/g, "\\|");
  r = r.replace(/\$/g, "\\$");
  r = r.replace(/\^/g, "\\^");

  r = r.replace(/#\*/g, "\\u{002A}");
  r = r.replace(/#\+/g, "\\u{002B}");

  r = r.replace(/\*/g, "[\\s\\S]*");
  r = r.replace(/\+/g, "[\\s\\S]");

  const reg = new RegExp("^" + r + "$", "iu");

  return l.match(reg) !== null;
}