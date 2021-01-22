import {ICharacter} from "../types/_character";

export function translate(input: ICharacter, c: string): void {
  if (c === "LOWER") {
    input.set(input.get().toLowerCase());
  } else if (c === "UPPER") {
    input.set(input.get().toUpperCase());
  } else {
    const chunks = c.match(/.{1,2}/g);
    for (const chunk of chunks || []) {
      const search = chunk.substr(0, 1);
      const replace = chunk.substr(1, 1);
      input.set(input.get().replace(new RegExp(search, "g"), replace));
    }
  }
}