import {Structure} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function co(left: number | string | ICharacter | INumeric, right: string | Structure | ICharacter): boolean {
  let l = "";
  if (typeof left === "number" || typeof left === "string") {
    l = left.toString();
  } else {
    l = left.get().toString();
  }

  let r = "";
  if (typeof right === "string") {
    r = right.toString();
  } else if (right instanceof Structure) {
    r = right.getCharacter();
  } else {
    r = right.get().toString();
  }

  let fdpos = 0;
  for (const c of l) {
    if (r.includes(c) === false) {
      // @ts-ignore
      abap.builtin.sy.get().fdpos.set(fdpos);
      return false;
    }
    fdpos++;
  }
  // @ts-ignore
  abap.builtin.sy.get().fdpos.set(fdpos);

  return true;
}