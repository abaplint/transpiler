import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function cp(left: number | string | ICharacter | INumeric, right: string | ICharacter): boolean {
  let l = "";
  if (typeof left === "number" || typeof left === "string") {
    l = left.toString();
  } else {
    l = left.get().toString();
  }

  let r = "";
  if (typeof right === "string") {
    r = right.toString();
  } else {
    r = right.get().toString();
  }

  r = r.replace(/\\/g, "\\\\");
  r = r.replace(/\[/g, "\\[");
  r = r.replace(/\]/g, "\\]");
  r = r.replace(/\?/g, "\\?");
  r = r.replace(/\(/g, "\\(");
  r = r.replace(/\)/g, "\\)");
  r = r.replace(/\./g, "\\.");
  r = r.replace(/\|/g, "\\|");

  r = r.replace(/\*/g, "[\\s\\S]*");
  r = r.replace(/\+/g, "[\\s\\S]+");

  const reg = new RegExp("^" + r + "$", "i");

  return l.match(reg) !== null;
}