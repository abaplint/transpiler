import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IFindOptions {
  find?: ICharacter | string,
  regex?: string,
  offset?: INumeric,
  length?: INumeric,
  count?: INumeric,
}

export function find(input: ICharacter | string, options: IFindOptions) {

  let i = input;
  if (typeof i !== "string") {
    i = i.get();
  }

  let s: ICharacter | string | RegExp = "";
  if (options.find) {
    s = options.find;
    if (typeof s !== "string") {
      s = s.get();
    }
  } else if (options.regex) {
    s = new RegExp(options.regex);
  }

  const match = i.match(s);

  if (match === undefined || match === null) {
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(4);
  } else {
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(0);
    if (match?.index) {
      options.offset?.set(match.index);
    }
    if (options?.count) {
      options.count?.set(1);
    }
    if (match[0]) {
      options.length?.set(match[0].length);
    }
  }

}