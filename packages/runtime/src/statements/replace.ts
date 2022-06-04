import {ICharacter} from "../types/_character";

export type replaceInput = {
  target: ICharacter,
  sectionLength?: ICharacter,
  regex?: ICharacter,
  all: boolean,
  with: ICharacter,
  of: ICharacter,
  ignoringCase?: boolean,
};

export function replace(input: replaceInput): void {
  let temp = input.target.get();

  const ignoreCase = input.ignoringCase === true ? "i" : "";

  let search: RegExp | string = "";
  let found = false;
  if (input.of) {
    search = input.of.get();
    if (search.length === 0 && input.all === true) {
      throw "REPLACE, zero length input";
    }
    found = temp.indexOf(search) >= 0;
    if (ignoreCase.length > 0) {
      search = new RegExp(search, ignoreCase);
    }
  } else if (input.regex) {
    if (input.regex.get().length === 0 && input.all === true) {
      throw "REPLACE, zero length input";
    }
    found = temp.match(search) !== null;
    search = new RegExp(input.regex.get(), ignoreCase);
  }

  let replace: string = "";
  if (typeof input.with === "string") {
    replace = input.with;
  } else {
    replace = input.with.get();
    replace = replace.replace(/\\\$/g, "$");
    replace = replace.replace(/\\\{/g, "{");
    replace = replace.replace(/\\\}/g, "}");
  }

  if (input.all === true) {
    if (input.regex) {
      temp = temp.replace(new RegExp(input.regex.get(), "g" + ignoreCase), replace);
    } else {
      while(temp.replace(search, replace) !== temp) {
        temp = temp.replace(search, replace);
      }
    }
  } else {
    temp = temp.replace(search, replace);
  }

  const subrc = found ? 0 : 4;
  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);

  input.target.set(temp);
}