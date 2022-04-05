import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function cs(left: number | string | ICharacter | INumeric, right: string | ICharacter): boolean {
  let l = "";
  if (typeof left === "number" || typeof left === "string") {
    l = left.toString();
  } else {
    l = left.get().toString();
  }
  l = l.toUpperCase();

  let r = "";
  if (typeof right === "string") {
    r = right.toString();
  } else {
    r = right.get().toString();
  }
  r = r.toUpperCase();

  const index = l.indexOf(r);
  if (index < 0) {
    // @ts-ignore
    abap.builtin.sy.get().fdpos.set(l.length);
    return false;
  } else {
    // @ts-ignore
    abap.builtin.sy.get().fdpos.set(index);
    return true;
  }
}