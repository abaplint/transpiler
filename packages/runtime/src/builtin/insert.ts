import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IInsertInput {
  val: ICharacter;
  sub: ICharacter;
  off?: INumeric;
}

export function insert(_input: IInsertInput) {
// todo
  return new String().set("todo");
}