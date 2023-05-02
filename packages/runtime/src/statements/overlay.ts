import {OffsetLength} from "../offset_length.js";
import {Character, Structure} from "../types/index.js";
import {ICharacter} from "../types/_character.js";

export function overlay(value: Character | Structure, withh: ICharacter, _only?: ICharacter) {
  const set = value instanceof Structure ? value.getCharacter() : value.get();
  const w = withh.get();
  const len = set.length;

  for (let i = 0; i < len; i++) {
    if (set.substring(i, i + 1) === " ") {
      new OffsetLength(value, {offset: i, length: 1}).set(w.substring(i, i + 1));
    }
  }
}