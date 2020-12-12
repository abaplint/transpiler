import {FieldSymbol} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IAssignInput {
  source: INumeric | ICharacter,
  target: FieldSymbol,
}

export function assign(input: IAssignInput) {
  input.target.assign(input.source);
}