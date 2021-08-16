import {Integer, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IFindOptions {
  find?: ICharacter | string,
  first?: boolean,
  regex?: string | ICharacter,
  offset?: INumeric,
  sectionOffset?: INumeric,
  byteMode?: boolean,
  length?: INumeric,
  count?: INumeric,
  results?: Table | Structure,
  ignoringCase?: boolean,
  submatches?: ICharacter[],
}

export function find(input: ICharacter | string, options: IFindOptions) {
  let i = input;
  if (typeof i !== "string") {
    i = i.get();
  }

  let sectionOffset = options.sectionOffset?.get();
  if (sectionOffset && options.byteMode) {
    sectionOffset = sectionOffset * 2;
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

    s = s.replace(/\[/g, "\\[");
    s = s.replace(/\]/g, "\\]");
    s = s.replace(/\?/g, "\\?");
    s = s.replace(/\(/g, "\\(");
    s = s.replace(/\)/g, "\\)");
    s = s.replace(/\./g, "\\.");
    s = s.replace(/\|/g, "\\|");
    s = s.replace(/\*/g, "\\*");
    s = s.replace(/\+/g, "\\+");

    s = new RegExp(s, "g");
  } else if (options.regex) {
    if (options.regex === "") {
      throw "FIND, runtime, no input, regex empty";
    }
    let r = options.regex;
    if (typeof r !== "string") {
      r = r.get();
    }
    // check type, it can also be a CL_ABAP_REGEX
    if (typeof r === "string") {
      r = r.replace("[[:space:]]", "\\s");
    }
    s = new RegExp(r, "g" + (options.ignoringCase === true ? "i" : ""));
  } else {
    throw "FIND, runtime, no input";
  }

  if (sectionOffset) {
    i = i.substr(sectionOffset);
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

  if (options.results) {
// assumption, results is a table with the correct type
    options.results.clear();
    for (const m of matches) {
      const match = new Structure({
        line: new Integer(),
        offset: new Integer(),
        length: new Integer(),
        submatches: new Table(new Structure({offset: new Integer(), length: new Integer()})),
      });

      match.get().line.set(0); // todo
      match.get().offset.set(m.index);
      match.get().length.set(m[0].length);

      if (m.length === 2) {
        const submatch = new Structure({offset: new Integer(), length: new Integer()});
        if (m[1] === undefined) {
          submatch.get().offset.set(-1);
          submatch.get().length.set(0);
        } else {
          submatch.get().offset.set(m.index + m[0].indexOf(m[1]));
          submatch.get().length.set(m[1].length);
        }
        match.get().submatches.append(submatch);
      }

      if (options.results instanceof Table) {
        options.results.append(match);
      } else {
        options.results.set(match);
      }
      if (options.first === undefined || options.first === true) {
        break;
      }
    }
  }

  if (matches.length === 0) {
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(4);
  } else {
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(0);
  }

  if (matches[0]?.index !== undefined) {
    let val = matches[0].index;
    if (sectionOffset) {
      val += sectionOffset;
    }
    if (options.byteMode) {
      val = val / 2;
    }
    options.offset?.set(val);
  }

  if (options?.count) {
    options.count?.set(matches.length);
  } else {
    options.count?.clear();
  }

  if (options?.length && matches && matches[0]) {
    options.length?.set(matches[0][0].length);
  } else {
    options.length?.clear();
  }

}