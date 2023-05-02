import {Structure} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";

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
    r = Object.values(right.get()).map((a: any) => a.get()).join("");
  } else {
    r = right.get().toString();
  }

  const characters = r.split("");

  let fdpos = 0;
  for (const c of l.split("")) {
    if (characters.includes(c) === false) {
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