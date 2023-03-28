import {Character, String, FieldSymbol, Hex, Table, ABAPObject, Structure, DataReference, Integer, Float, Numc, XString, Packed, Time, Date, DecFloat34, HashedTable} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IDescribeOptions {
  field: any,
  type?: ICharacter,
  length?: INumeric,
  decimals?: INumeric,
  lines?: INumeric,
  table?: Table,
  mode?: "BYTE" | "CHARACTER"
}

export function describe(input: IDescribeOptions) {
//  console.dir(input);
  if (input.type) {
    if (input.field instanceof FieldSymbol) {
      describe({field: input.field.getPointer(), type: input.type, length: input.length, mode: input.mode});
      return;
    }
    if (input.field instanceof Table || input.field instanceof HashedTable) {
      input.type.set("h");
    } else if (input.field instanceof Character || typeof input.field === "string") {
      input.type.set("C");
    } else if (input.field instanceof Integer) {
      input.type.set("I");
    } else if (input.field instanceof Date) {
      input.type.set("D");
    } else if (input.field instanceof Time) {
      input.type.set("T");
    } else if (input.field instanceof Float) {
      input.type.set("F");
    } else if (input.field instanceof Numc) {
      input.type.set("N");
    } else if (input.field instanceof Hex) {
      input.type.set("X");
    } else if (input.field instanceof Packed) {
      input.type.set("P");
    } else if (input.field instanceof String) {
      input.type.set("g");
    } else if (input.field instanceof XString) {
      input.type.set("y");
    } else if (input.field instanceof DecFloat34) {
      input.type.set("e");
    } else if (input.field instanceof Structure) {
      input.type.set("u");
    } else if (input.field instanceof ABAPObject) {
      input.type.set("r");
    } else if (input.field instanceof DataReference) {
      input.type.set("l");
    } else {
      throw new Error("DESCRIBE, todo, transpiler, " + input.field.constructor.name);
    }
  }

  if (input.field instanceof FieldSymbol) {
    input.field = input.field.getPointer();
  }

  if (input.length) {
    if (input.field instanceof Character
        || input.field instanceof Packed
        || input.field instanceof Hex) {
      input.length.set(input.field.getLength());
    } else {
      throw "DESCRIBE length, unsupported or todo";
    }
  }

  if (input.decimals) {
    if (input.field instanceof Packed) {
      input.decimals.set(input.field.getDecimals());
    } else {
      throw "DESCRIBE decimals, unsupported or todo";
    }
  }

  if (input.table) {
    // @ts-ignore
    abap.builtin.sy.get().tfill.set(input.table.getArrayLength());

    input.lines?.set(input.table.getArrayLength());
  }
}