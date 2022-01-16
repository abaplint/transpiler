import {Structure} from "../types";
import {ICharacter} from "../types/_character";

export interface IInsertDatabaseOptions {
  values: Structure,
}

export function insertDatabase(_table: string | ICharacter, _options: IInsertDatabaseOptions): void {
  // @ts-ignore
  abap.builtin.sy.get().subrc.set(0);
}