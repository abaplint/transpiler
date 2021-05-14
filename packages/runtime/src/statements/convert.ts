import {ICharacter} from "../types/_character";

export interface IConvertSource {
  date: ICharacter | string,
  time: ICharacter | string,
  zone: ICharacter | string,
}

export interface IConvertTarget {
  stamp: ICharacter | string,
}

export function convert(_source: IConvertSource, _target: IConvertTarget) {
  console.dir("todo");
}