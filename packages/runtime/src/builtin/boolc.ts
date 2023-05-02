import {Character, String} from "../types/index.js";

export function boolc(input: boolean | undefined | {val: Character | String}) {
  if (input === true) {
    return new String().set("X");
  } else if (input === false || input === undefined) {
    return new String().set(" ");
  } else if (input.val instanceof String && input.val.get().trim() === "") {
    return new String().set(" ");
  } else if (input.val instanceof Character && input.val.get().trim() === "") {
    return new String().set(" ");
  } else {
    return new String().set("X");
  }
}