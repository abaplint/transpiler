import {ICharacter} from "../types/_character";
import {setText} from "./_set_text";

export function translate(input: ICharacter, i: ICharacter | string): void {
  let c = i;
  if (typeof c !== "string") {
    c = c.get();
  }
  if (c === "LOWER") {
    setText(input, input.get().toLowerCase());
  } else if (c === "UPPER") {
    setText(input, input.get().toUpperCase());
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
    setText(input, result);
  }
}