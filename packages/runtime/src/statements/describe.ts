import {Character, String, FieldSymbol, Hex, Table, ABAPObject, Structure, DataReference, Integer, Float, Numc, XString, Packed, Time, Date, DecFloat34, HashedTable, Integer8, HexUInt8, UTCLong} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IDescribeOptions {
  field: any,
  type?: ICharacter,
  length?: INumeric,
  decimals?: INumeric,
  lines?: INumeric,
  table?: Table,
  mode?: "BYTE" | "CHARACTER"
}

// a structure is deep when a component is a string, a table or a reference,
// or a deep structure itself: type kind v, a flat structure is u
function isDeep(structure: Structure): boolean {
  for (const component of Object.values(structure.get())) {
    if (component instanceof String
        || component instanceof XString
        || component instanceof Table
        || component instanceof HashedTable
        || component instanceof DataReference
        || component instanceof ABAPObject
        || (component instanceof Structure && isDeep(component))) {
      return true;
    }
  }
  return false;
}

/** the length in bytes of a field and its alignment, as in a Unicode system: a character takes 2 bytes; strings,
 *  tables and references are 8-byte references; a structure is its components with alignment gaps */
function byteLength(field: any): {length: number, align: number} {
  if (field instanceof Character || field instanceof Numc) {
    return {length: field.getLength() * 2, align: 2};
  } else if (field instanceof Date) {
    return {length: 16, align: 2};
  } else if (field instanceof Time) {
    return {length: 12, align: 2};
  } else if (field instanceof Integer) {
    return {length: 4, align: 4};
  } else if (field instanceof Integer8 || field instanceof Float || field instanceof UTCLong) {
    return {length: 8, align: 8};
  } else if (field instanceof DecFloat34) {
    return {length: 16, align: 8};
  } else if (field instanceof Packed || field instanceof Hex || field instanceof HexUInt8) {
    return {length: field.getLength(), align: 1};
  } else if (field instanceof Structure) {
    let length = 0;
    let align = 1;
    const seen = new Set<any>();
    for (const component of Object.values(field.get())) {
      if (seen.has(component)) {
        continue;
      }
      seen.add(component);
      const c = byteLength(component);
      length = Math.ceil(length / c.align) * c.align + c.length;
      align = Math.max(align, c.align);
    }
    return {length: Math.ceil(length / align) * align, align};
  }
  // strings, tables and references: the field holds a reference
  return {length: 8, align: 8};
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
    } else if (input.field instanceof Integer8) {
      input.type.set("8");
    } else if (input.field instanceof Date) {
      input.type.set("D");
    } else if (input.field instanceof Time) {
      input.type.set("T");
    } else if (input.field instanceof Float) {
      input.type.set("F");
    } else if (input.field instanceof Numc) {
      input.type.set("N");
    } else if (input.field instanceof Hex || input.field instanceof HexUInt8) {
      input.type.set("X");
    } else if (input.field instanceof Packed) {
      input.type.set("P");
    } else if (input.field instanceof String) {
      input.type.set("g");
    } else if (input.field instanceof XString) {
      input.type.set("y");
    } else if (input.field instanceof UTCLong) {
      input.type.set("p");
    } else if (input.field instanceof DecFloat34) {
      input.type.set("e");
    } else if (input.field instanceof Structure) {
      input.type.set(isDeep(input.field) ? "v" : "u");
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
    const characters = input.field instanceof Character || input.field instanceof Numc
      || input.field instanceof Date || input.field instanceof Time;
    if (input.mode === "CHARACTER" && characters) {
      input.length.set(input.field instanceof Date ? 8 : input.field instanceof Time ? 6 : input.field.getLength());
    } else if (input.mode === "CHARACTER" && input.field instanceof Structure) {
      // a flat structure of character-like components only
      input.length.set(byteLength(input.field).length / 2);
    } else if (input.mode === "BYTE" || input.mode === undefined) {
      input.length.set(input.mode === undefined && characters ? byteLength(input.field).length / 2 : byteLength(input.field).length);
    } else {
      throw new Error("DESCRIBE length, unsupported or todo, " + input.field.constructor.name);
    }
  }

  if (input.decimals) {
    if (input.field instanceof Packed) {
      input.decimals.set(input.field.getDecimals());
    } else {
      throw new Error("DESCRIBE decimals, unsupported or todo, " + input.field.constructor.name);
    }
  }

  if (input.table) {
    abap.builtin.sy.get().tfill.set(input.table.getArrayLength());

    input.lines?.set(input.table.getArrayLength());
  }
}
