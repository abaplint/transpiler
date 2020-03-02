import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {Console} from "../console";

export function write(source: INumeric | ICharacter | string | number) {
  if (typeof source === "string" || typeof source === "number") {
    Console.add(source.toString());
  } else {
    Console.add(source.get().toString());
  }
}