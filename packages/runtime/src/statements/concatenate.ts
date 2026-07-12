import {INumeric} from "../types/_numeric";
import {ICharacter} from "../types/_character";
import {Character, Table} from "../types";

export interface IConcatenateInput {
  source: (number | string | INumeric | ICharacter | Table)[],
  target: ICharacter,
  separatedBy?: number | string | INumeric | ICharacter,
  respectingBlanks?: boolean,
  lines?: boolean,
  byteMode?: boolean,
}

export function concatenate(input: IConcatenateInput) {

  if (input.byteMode === true) {
    concatenateByteMode(input);
    return;
  }

  let sep = "";
  if (input.separatedBy) {
    if (typeof input.separatedBy === "string" || typeof input.separatedBy === "number") {
      sep = input.separatedBy.toString();
    } else {
      sep = input.separatedBy.get().toString();
    }
  }

  if (input.lines === true) {
    const list: string[] = [];
    const tab = input.source[0];
    if (tab instanceof Table) {
      for (const l of tab.array()) {
        if (input.respectingBlanks !== true) {
          list.push(l.get().trimEnd());
        } else {
          list.push(l.get());
        }
      }
    }
    input.target.set(list.join(sep));

  } else {
    let result = "";

    for (const source of input.source) {
      let val = "";
      if (source instanceof Table) {
        throw new Error("concatenate, error: input is table");
      } else if (typeof source === "string" || typeof source === "number") {
        val = source.toString();
      } else if (source instanceof Character) {
        if (input.respectingBlanks !== true) {
          val = source.getTrimEnd();
        } else {
          val = source.get();
        }
      } else {
        val = source.get().toString();
      }
      result += val + sep;
    }

    if (sep.length > 0 && result.length > 0) {
      result = result.slice(0, result.length - sep.length);
    }

    input.target.set(result);
  }

}

function rawByteValue(source: number | string | INumeric | ICharacter): string {
  if (typeof source === "string") {
    return source;
  } else if (typeof source === "number") {
    return source.toString();
  } else {
    return source.get().toString();
  }
}

function concatenateByteMode(input: IConcatenateInput) {

  let sep = "";
  if (input.separatedBy !== undefined) {
    sep = rawByteValue(input.separatedBy);
  }

  let result = "";

  if (input.lines === true) {
    const tab = input.source[0];
    if (tab instanceof Table) {
      for (const l of tab.array()) {
        result += l.get().toString() + sep;
      }
    }
  } else {
    for (const source of input.source) {
      if (source instanceof Table) {
        throw new Error("concatenate, error: input is table");
      }
      result += rawByteValue(source) + sep;
    }
  }

  if (sep.length > 0 && result.length > 0) {
    result = result.slice(0, result.length - sep.length);
  }

  input.target.set(result);
}
