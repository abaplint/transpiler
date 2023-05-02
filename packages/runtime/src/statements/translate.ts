import {ICharacter} from "../types/_character.js";

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
    for (const chunk of chunks || []) {
      let search = chunk.substr(0, 1);
      const replace = chunk.substr(1, 1);

      // regexp escaping
      if (search === "+"
          || search === "*"
          || search === "?"
          || search === "."
          || search === "^"
          || search === "$"
          || search === "|"
          || search === "["
          || search === "]"
          || search === "\\"
          || search === "("
          || search === ")") {
        search = "\\" + search;
      }

      input.set(input.get().replace(new RegExp(search, "g"), replace));
    }
  }
}