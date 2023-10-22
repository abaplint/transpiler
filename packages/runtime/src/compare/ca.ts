import {FieldSymbol, Structure} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function ca(left: number | string | ICharacter | INumeric | Structure, right: string | ICharacter): boolean {
  if (left instanceof FieldSymbol) {
    return ca(left.getPointer(), right);
  } else if (right instanceof FieldSymbol) {
    return ca(left, right.getPointer());
  }

  let l = "";
  if (typeof left === "number" || typeof left === "string") {
    l = left.toString();
  } else if (left instanceof Structure) {
    l = left.getCharacter();
  } else {
    l = left.get().toString();
  }
  if (l === "") {
    l = " ";
  }

  let r = "";
  if (typeof right === "string") {
    r = right.toString();
  } else {
    r = right.get().toString();
  }

  let fdpos = 0;
  for (const c of l) {
    if (r.includes(c) === true) {
      // @ts-ignore
      abap.builtin.sy.get().fdpos.set(fdpos);
      return true;
    }
    fdpos++;
  }
  // @ts-ignore
  abap.builtin.sy.get().fdpos.set(fdpos);

  return false;
}