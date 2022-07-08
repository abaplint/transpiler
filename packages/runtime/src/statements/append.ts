import {FieldSymbol, Table, TableRowType} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IAppendOptions {
  source: TableRowType,
  target: Table,
  lines?: boolean,
  assigning?: FieldSymbol,
  from?: ICharacter | INumeric,
  to?: ICharacter | INumeric,
}

export function append(input: IAppendOptions) {
  if (input.lines === true && input.source instanceof Table) {
    let from = 1;
    if (input.from) {
      from = parseInt(input.from.get() + "", 10);
    }
    let to = input.source.array().length;
    if (input.to) {
      to = parseInt(input.to.get() + "", 10);
    }
    let index = 1;
    for (const a of input.source.array()) {
      if (index < from || index > to) {
        index++;
        continue;
      }
      input.target.append(a);
      index++;
    }
  } else {
    const val = input.target.append(input.source);
    if (input.assigning) {
      if (val instanceof FieldSymbol) {
        input.assigning.assign(val.getPointer());
      } else {
        input.assigning.assign(val);
      }
    }
  }

  // @ts-ignore
  abap.builtin.sy.get().tabix.set(input.target.array().length);
}