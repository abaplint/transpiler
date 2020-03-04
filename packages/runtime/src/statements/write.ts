import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {Console} from "../console";

export function write(source: INumeric | ICharacter | string | number, options?: {newLine?: boolean}) {
  if (options?.newLine === true && Console.get().length > 0) {
    Console.add("\n");
  }
  if (typeof source === "string" || typeof source === "number") {
    Console.add(source.toString());
  } else {
    Console.add(source.get().toString());
  }
}