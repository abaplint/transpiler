import {INumeric} from "../types/_numeric";
import {ICharacter} from "../types/_character";
import {Character, FieldSymbol, Hex, String, Table, XString} from "../types";
import {ABAP} from "..";
import {setText} from "./_set_text";

declare const abap: ABAP;

export interface IConcatenateInput {
  source: (number | string | INumeric | ICharacter | Table)[],
  target: ICharacter,
  separatedBy?: number | string | INumeric | ICharacter,
  respectingBlanks?: boolean,
  lines?: boolean,
  byteMode?: boolean,
}

// sy-subrc 4 when a c target cuts the result, else 0
function setTarget(target: ICharacter, result: string) {
  let subrc = 0;
  const pointer = target instanceof FieldSymbol ? target.getPointer() : target;
  if (pointer instanceof Character && result.trimEnd().length > pointer.getLength()) {
    subrc = 4;
  }
  setText(target, result);
  abap.builtin.sy.get().subrc.set(subrc);
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
    let result = "";
    const tab = input.source[0];
    if (tab instanceof Table) {
      const array = tab.array();
      const length = array.length;
      const respectingBlanks = input.respectingBlanks === true;
      const hasSeparator = sep.length > 0;
      for (let i = 0; i < length; i++) {
        if (hasSeparator === true && i > 0) {
          result += sep;
        }
        const row = array[i];
        const value = row.get();
        // Preserve trailing blanks in string rows even without RESPECTING BLANKS.
        result += respectingBlanks === true || row instanceof String ? value : value.trimEnd();
      }
    }
    setTarget(input.target, result);

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

    setTarget(input.target, result);
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

// the value of an xstring or x field is hexadecimal digits already, uppercase and even in number
function isHex(source: unknown): boolean {
  const value = source instanceof FieldSymbol ? source.getPointer() : source;
  return value instanceof XString || value instanceof Hex;
}

function concatenateByteMode(input: IConcatenateInput) {

  let sep = "";
  // when every part is an xstring or an x field, the result needs no check for an
  // xstring target; XString.set( ) checks the whole string, so appending to an
  // xstring in a loop took time in the square of its length
  let hex = input.separatedBy === undefined || isHex(input.separatedBy);
  if (input.separatedBy !== undefined) {
    sep = rawByteValue(input.separatedBy);
  }

  let result = "";

  if (input.lines === true) {
    const tab = input.source[0];
    if (tab instanceof Table) {
      for (const l of tab.array()) {
        hex = hex && isHex(l);
        result += l.get().toString() + sep;
      }
    }
  } else {
    for (const source of input.source) {
      if (source instanceof Table) {
        throw new Error("concatenate, error: input is table");
      }
      hex = hex && isHex(source);
      result += rawByteValue(source) + sep;
    }
  }

  if (sep.length > 0 && result.length > 0) {
    result = result.slice(0, result.length - sep.length);
  }

  const target = input.target instanceof FieldSymbol ? input.target.getPointer() : input.target;
  if (hex === true && target instanceof XString) {
    target.setHex(result);
    abap.builtin.sy.get().subrc.set(0);
    return;
  }
  setTarget(input.target, result);
}
