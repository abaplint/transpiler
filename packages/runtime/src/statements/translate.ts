import {ICharacter} from "../types/_character";

export function translate(input: ICharacter, i: ICharacter | string): void {
  let c = i;
  if (typeof c !== "string") {
    c = c.get();
  }
  if (c === "LOWER") {
    input.set(input.get().toLowerCase());
  } else if (c === "UPPER") {
    input.set(input.get().toUpperCase());
  } else {
    const chunks = c.match(/.{1,2}/g);
    const characters = input.get().split("");
    let result = "";
    for (let char of characters) {
      for (const chunk of chunks || []) {
        const search = chunk.substr(0, 1);
        const replace = chunk.substr(1, 1);

        if (char === search) {
          char = replace;
          break;
        }
      }
      result += char;
    }
    input.set(result);
  }
}