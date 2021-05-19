import {Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function find(input: {val: ICharacter | string, sub: ICharacter | string, off: INumeric | number}) {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  const sub = typeof input.sub === "string" ? input.sub : input.sub.get();
  const off = typeof input.off === "number" ? input.off : input.off.get();

  const found = val.indexOf(sub, off);

  return new Integer().set(found);
}