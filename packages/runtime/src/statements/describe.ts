import {Character, String, FieldSymbol, Hex, Table, ABAPObject} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IDescribeOptions {
  field: any,
  type?: ICharacter,
  length?: INumeric,
  mode?: "BYTE" | "CHARACTER"
}

export function describe(input: IDescribeOptions) {
  if (input.type) {
    if (input.field instanceof FieldSymbol) {
      describe({field: input.field.getPointer(), type: input.type, length: input.length, mode: input.mode});
      return;
    }
    if (input.field instanceof Table) {
      input.type.set("h");
    } else if (input.field instanceof Character || typeof input.field === "string") {
      input.type.set("C");
    } else if (input.field instanceof String) {
      input.type.set("g");
    } else if (input.field instanceof ABAPObject) {
      input.type.set("r");
    }
    // todo
  }

  if (input.length) {
    if (input.field instanceof FieldSymbol) {
      input.field = input.field.getPointer();
    }
    if (input.field instanceof Character
        || input.field instanceof Hex) {
      input.length.set(input.field.getLength());
    } else {
      throw "DESCRIBE, unsupported or todo";
    }
  }
}