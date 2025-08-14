import {ICharacter} from "../types/_character";

export function line_exists(callback: () => void): ICharacter {

  try {
    callback();
  } catch (error) {
    throw new Error("runtime line_exists() todo");
  }

  throw new Error("runtime line_exists() todo");
}