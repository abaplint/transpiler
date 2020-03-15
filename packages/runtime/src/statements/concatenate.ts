import {INumeric} from "../types/_numeric";
import {ICharacter} from "../types/_character";

export function concatenate(input: {source: [number | string | INumeric | ICharacter], target: ICharacter}) {
  let res = "";

  for (const source of input.source) {
    if (typeof source === "string" || typeof source === "number") {
      res = res + source.toString();
    } else {
      res = res + source.get().toString();
    }
  }

  input.target.set(res);
}