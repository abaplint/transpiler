import {ICharacter} from "../types/_character";

export function translate(input: ICharacter, c: "UPPER" | "LOWER"): void {
  if (c === "LOWER") {
    input.set(input.get().toLowerCase());
  } else {
    input.set(input.get().toUpperCase());
  }
}