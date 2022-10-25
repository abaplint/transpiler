import {OffsetLength} from "../offset_length";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export type replaceInput = {
  target: ICharacter,
  sectionLength?: INumeric,
  sectionOffset?: INumeric,
  regex?: ICharacter,
  all: boolean,
  with: ICharacter,
  of: ICharacter,
  ignoringCase?: boolean,
};

function escapeRegExp(text: string) {
  return text.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, "\\$&");
}

export function replace(input: replaceInput): void {
  let temp = input.target.get();

  const ignoreCase = input.ignoringCase === true ? "i" : "";
  const allOccurrences = input.all === true ? "g" : "";

  let search: RegExp | undefined = undefined;
  let found = false;
  if (input.of) {
    let inp = input.of.get();
    if (inp.length === 0 && input.all === true) {
      throw "REPLACE, zero length input";
    }
    found = temp.indexOf(inp) >= 0;
    inp = escapeRegExp(inp);
    search = new RegExp(inp, ignoreCase + allOccurrences);
  } else if (input.regex) {
    if (input.regex.get().length === 0 && input.all === true) {
      throw "REPLACE, zero length input";
    }
    found = temp.match(input.regex.get()) !== null;
    search = new RegExp(input.regex.get(), ignoreCase + allOccurrences);
  } else if (input.sectionLength && input.sectionOffset) {
    new OffsetLength(input.target, {length: input.sectionLength, offset: input.sectionOffset}).set(input.with);
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(0);
    return;
  } else {
    throw "REPLACE, unexpected input";
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

  temp = temp.replace(search, replace);

  const subrc = found ? 0 : 4;
  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);

  input.target.set(temp);
}