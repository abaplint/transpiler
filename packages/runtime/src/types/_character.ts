import {Hex} from "./hex";
import {INumeric} from "./_numeric";

export interface ICharacter {
  set(value: ICharacter | string): void;
  get(): string;
  clear(): void;
  getOffset(input: {offset?: number | INumeric | Hex, length?: number | INumeric | Hex}): ICharacter;
  clone(): ICharacter;
}