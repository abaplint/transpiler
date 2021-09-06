import {String} from "../types";

export function boolc(input: boolean | undefined) {
  if (input === true) {
    return new String().set("X");
  } else if (input === false || input === undefined) {
    return new String().set("");
  } else {
    return new String().set("X");
  }
}