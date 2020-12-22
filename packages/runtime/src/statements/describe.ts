import {Character, FieldSymbol, Hex, Table} from "../types";
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
    if (input.field instanceof Table) {
      input.type.set("h");
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