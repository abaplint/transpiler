import {ABAPRegExp} from "../abap_regex";
import {Character, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {concatenate} from "./concatenate";
import {IRegexOptions} from "./find";
import {ABAP} from "..";

declare const abap: ABAP;

export type replaceInput = IRegexOptions & {
  target: ICharacter | Table,
  sectionLength?: INumeric,
  sectionOffset?: INumeric,
  replacementLength?: INumeric,
  replacementCount?: INumeric,
  with: ICharacter,
  of: ICharacter,
};

export function replace(input: replaceInput): void {
  if (input.target instanceof Table) {
    for (const row of input.target.array()) {
      replace({...input, target: row});
    }
    return;
  }

  let temp = input.target.get();

  const ignoreCase = input.ignoringCase === true ? "i" : "";
  const allOccurrences = input.all === true ? "g" : "";

  let search: RegExp | undefined = undefined;
  let found = false;
  if (input.of) {
    let inp = input.of.get();
    if (input.of instanceof Character) {
      inp = input.of.getTrimEnd();
    }

    if (inp.length === 0 && input.all === true) {
      throw "REPLACE, zero length input";
    }
    found = temp.indexOf(inp) >= 0;
    inp = ABAPRegExp.escapeRegExp(inp);
    search = new RegExp(inp, ignoreCase + allOccurrences);
  } else if (input.regex || input.pcre) {
    search = ABAPRegExp.getRegex(input);
    found = temp.match(search) !== null;
  } else if (input.sectionLength && input.sectionOffset) {
    const before = input.target.getOffset({length: input.sectionOffset});
    const after = input.target.getOffset({offset: input.sectionLength.get() + input.sectionOffset.get()});
    concatenate({source: [before, input.with, after], target: input.target});
    abap.builtin.sy.get().subrc.set(0);
    return;
  } else {
    throw "REPLACE, unexpected input";
  }

  let rr: string = "";
  if (typeof input.with === "string") {
    rr = input.with;
  } else {
    if (input.with instanceof Character) {
      rr = input.with.getTrimEnd();
    } else {
      rr = input.with.get();
    }
    rr = rr.replace(/\\\$/g, "$");
    rr = rr.replace(/\\\{/g, "{");
    rr = rr.replace(/\\\}/g, "}");
  }

  if (input.replacementLength) {
    const match = temp.match(search);
    let replacement = rr;
    for (let counter = 1; counter < 10; counter++) {
      const dollar = "$" + counter;
      if (replacement.includes(dollar) && match && match[counter] !== undefined) {
        replacement = replacement.replace(dollar, match[counter]);
      }
    }
    input.replacementLength.set(replacement.length);
  }

  if (input.replacementCount) {
    const match = temp.match(search);
    input.replacementCount.set(match?.length || 0);
  }

  temp = temp.replace(search, rr);

  const subrc = found ? 0 : 4;
  abap.builtin.sy.get().subrc.set(subrc);

  input.target.set(temp);
}