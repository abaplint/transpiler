import {Hex} from "./hex.js";
import {INumeric} from "./_numeric.js";

export interface ICharacter {
  set(value: ICharacter | string): void;
  get(): string;
  clear(): void;
  getOffset(input: {offset?: number | INumeric | Hex, length?: number | INumeric | Hex}): ICharacter;
}