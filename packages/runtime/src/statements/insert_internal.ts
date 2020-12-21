import {ABAPObject, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IInsertInternalOptions {
  index?: INumeric,
}

export function insertInternal(
  data: INumeric | ICharacter | Structure | ABAPObject,
  target: Table,
  options?: IInsertInternalOptions): void {

  if (options?.index) {
    const index = options.index.get() - 1;
    target.insertIndex(data, index);
  } else {
// todo, for now it just appends, this is not correct, but currently the table type is not known
    target.insertIndex(data, target.array().length - 1);
  }

}