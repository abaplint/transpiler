import {Character, Table} from "../types";
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
    } else {
      throw "DESCRIBE, todo";
    }
  } else if (input.length) {
    if (input.field instanceof Character) {
      input.length.set(input.field.getLength());
    } else {
      throw "DESCRIBE, unsupported or todo";
    }
  }
}