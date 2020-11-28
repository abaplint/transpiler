import {INumeric} from "../types/_numeric";
import {ICharacter} from "../types/_character";

export interface IConcatenateInput {
  source: [number | string | INumeric | ICharacter],
  target: ICharacter,
  separatedBy?: number | string | INumeric | ICharacter,
}

export function concatenate(input: IConcatenateInput) {
  const list: string[] = [];

  for (const source of input.source) {
    if (typeof source === "string" || typeof source === "number") {
      list.push(source.toString());
    } else {
      list.push(source.get().toString());
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