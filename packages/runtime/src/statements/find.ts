import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IFindOptions {
  find?: ICharacter | string,
  first?: boolean,
  regex?: string,
  offset?: INumeric,
  length?: INumeric,
  count?: INumeric,
  submatches?: ICharacter[],
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
    if (s === "") {
      throw "FIND, runtime, no input, s empty";
    }
    s = new RegExp(s, "g");
  } else if (options.regex) {
    if (options.regex === "") {
      throw "FIND, runtime, no input, regex empty";
    }
    s = new RegExp(options.regex, "g");
  } else {
    throw "FIND, runtime, no input";
  }

  let temp: RegExpExecArray | null;
  const matches: RegExpExecArray[] = [];
  // eslint-disable-next-line no-cond-assign
  while(temp = s.exec(i)) {
    matches.push(temp);
    if (options.first === true) {
      break;
    }
  }

  if (options.submatches) {
    for (let index = 0; index < options.submatches.length; index++) {
      if (matches[0] && matches[0][index + 1]) {
        options.submatches[index].set(matches[0][index + 1]);
      } else {
        options.submatches[index].clear();
      }
    }
  }

  if (matches.length === 0) {
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(4);
  } else {
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(0);
    if (matches[0]?.index) {
      options.offset?.set(matches[0].index);
    }
    if (options?.count) {
      options.count?.set(matches.length);
    }
    if (options?.length) {
      options.length?.set(matches[0][0].length);
    }
  }

}