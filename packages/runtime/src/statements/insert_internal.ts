import {ABAPObject, FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IInsertInternalOptions {
  index?: INumeric,
  assigning?: FieldSymbol,
  lines?: boolean,
}

export function insertInternal(
  data: INumeric | ICharacter | Structure | ABAPObject | Table,
  target: Table,
  options?: IInsertInternalOptions): void {

  if (options?.index) {
    const index = options.index.get() - 1;
    const val = target.insertIndex(data, index);
    if (options.assigning) {
      options.assigning.assign(val);
    }
  } else if (options?.lines && data instanceof Table) {
    for (const i of data.array()) {
      target.append(i);
    }
  } else {
// todo, for now it just appends, this is not correct, but currently the table type is not known
    const val = target.insertIndex(data, target.array().length);
    if (options?.assigning) {
      options.assigning.assign(val);
    }
  }

}