import {Structure} from "../types";
import {ICharacter} from "../types/_character";

export function overlay(value: ICharacter | Structure, _withh: ICharacter, _only?: ICharacter) {
  const set = value instanceof Structure ? value.getCharacter() : value.get();

  console.dir(set.length);

  value.set(set);
}