import {INumeric} from "../types/_numeric";
import {ICharacter} from "../types/_character";
import {Table} from "../types";

export interface IConcatenateInput {
  source: [number | string | INumeric | ICharacter | Table],
  target: ICharacter,
  separatedBy?: number | string | INumeric | ICharacter,
  respectingBlanks?: boolean,
  lines?: boolean,
}

export function concatenate(input: IConcatenateInput) {
  const list: string[] = [];

  if (input.lines === true) {
    const tab = input.source[0];
    if (tab instanceof Table) {
      for (const l of tab.array()) {
        list.push(l.get());
      }
    }
  } else {
    for (const source of input.source) {
      let val = "";
      if (source instanceof Table) {
        throw new Error("concatenate, error input is table");
      } else if (typeof source === "string" || typeof source === "number") {
        val = source.toString();
      } else {
        val = source.get().toString();
      }
      if (input.respectingBlanks !== true) {
        val = val.trimEnd();
      }
      list.push(val);
    }
  }

  let sep = "";
  if (input.separatedBy) {
    if (typeof input.separatedBy === "string" || typeof input.separatedBy === "number") {
      sep = input.separatedBy.toString();
    } else {
      sep = input.separatedBy.get().toString();
    }
  }

  input.target.set(list.join(sep));
}