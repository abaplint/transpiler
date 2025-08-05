import {DataReference, FieldSymbol, Table, TableRowType} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IAppendOptions {
  source: TableRowType | FieldSymbol | Table,
  target: Table | FieldSymbol | undefined,
  lines?: boolean,
  assigning?: FieldSymbol,
  referenceInto?: DataReference,
  from?: ICharacter | INumeric,
  to?: ICharacter | INumeric,
}

export function append(input: IAppendOptions) {
  if (input.target instanceof FieldSymbol) {
    input.target = input.target.getPointer() as Table | undefined;
    if (input.target === undefined) {
      throw "Field symbol not assigned";
    }
  }

  if (input.source instanceof FieldSymbol) {
    input.source = input.source.getPointer() as TableRowType;
  }

  if (input.target === undefined) {
    // short APPEND, use header field
    if (!(input.source instanceof Table)) {
      throw "APPEND, header, table";
    }
    input.source.append(input.source.getHeader());
    abap.builtin.sy.get().tabix.set(input.source.array().length);
    return;
  } else if (input.lines === true && input.source instanceof Table) {
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
    } else if (input.referenceInto) {
      if (val instanceof FieldSymbol) {
        input.referenceInto.assign(val.getPointer());
      } else {
        input.referenceInto.assign(val);
      }
    }
  }

  abap.builtin.sy.get().tabix.set(input.target.array().length);
}