import {Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IFindInput {
  val: ICharacter | string;
  sub?: ICharacter | string;
  off?: INumeric | number;
  regex?: ICharacter | string;
  case?: ICharacter | string;
}

export function find(input: IFindInput) {
  const val = typeof input.val === "string" ? input.val : input.val.get();

  if (input.regex) {
    const caseInput = typeof input.case === "string" ? input.case : input.case?.get();
    const regex = typeof input.regex === "string" ? input.regex : input.regex.get();

    const flags = caseInput !== "X" ? "i" : "";
    const reg = new RegExp(regex, flags);

    const ret = val.match(reg)?.index;
    if (ret !== undefined) {
      return new Integer().set(ret);
    } else {
      return new Integer().set(-1);
    }
  } else {
    const sub = typeof input.sub === "string" ? input.sub : input.sub?.get();
    const off = typeof input.off === "number" ? input.off : input.off?.get();

    const found = val.indexOf(sub || "", off);

    return new Integer().set(found);
  }
}