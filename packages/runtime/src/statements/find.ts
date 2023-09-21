import {ABAPRegExp} from "../abap_regex";
import {ABAPObject, Integer, Structure, Table, TableFactory} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IFindOptions {
  find?: ICharacter | string,
  first?: boolean,
  regex?: string | ICharacter | ABAPObject,
  pcre?: string | ICharacter,
  offset?: INumeric,
  sectionOffset?: INumeric,
  byteMode?: boolean,
  length?: INumeric,
  count?: INumeric,
  results?: Table | Structure,
  ignoringCase?: boolean,
  submatches?: ICharacter[],
}

export function find(input: ICharacter | Table, options: IFindOptions) {

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
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(0);
      return;
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
  } else if (options.regex || options.pcre) {
    if (options.regex) {
      if (typeof options.regex === "string") {
        if (options.regex === "") {
          throw new Error("FIND, runtime, no input, regex empty");
        }
      } else if (options.regex.get() === "") {
        throw new Error("FIND, runtime, no input, regex empty");
      }
    }

    let r = options.regex!;
    if (r === undefined) {
      r = options.pcre!;
    }
    if (typeof r !== "string") {
      r = r.get();
    }

    if (typeof r === "string") {
      r = ABAPRegExp.convert(r);
    } else if (r.constructor.name === "cl_abap_regex") {
      const obj = r;
      // @ts-ignore
      r = obj.mv_pattern.get();
      // @ts-ignore
      if (obj.mv_ignore_case.get() === "X") {
        options.ignoringCase = true;
      }
    } else {
      throw "find(), unexpected input";
    }

    s = new RegExp(r as string, "gm" + (options.ignoringCase === true ? "i" : ""));
  } else {
    throw "FIND, runtime, no input";
  }

  const matches = [];
  if (input instanceof Table) {
    let line = 1;
    for (const blah of input.array()) {
      let temp: RegExpExecArray | null;
      // eslint-disable-next-line no-cond-assign
      while(temp = s.exec(blah.get())) {
        matches.push({...temp, line});
        if (options.first === true) {
          break;
        }
      }
      line++;
    }
  } else {
    let blah = input.get();
    if (sectionOffset) {
      blah = blah.substr(sectionOffset);
    }

    let temp: RegExpExecArray | null;
    // eslint-disable-next-line no-cond-assign
    while(temp = s.exec(blah)) {
      matches.push(temp);
      if (options.first === true) {
        break;
      }
    }
  }

  if (options.submatches) {
    for (let index = 0; index < options.submatches.length; index++) {
// @ts-ignore
      if (matches[0] && matches[0][index + 1]) {
// @ts-ignore
        options.submatches[index].set(matches[0][index + 1]);
      } else if (matches.length > 0) {
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
        submatches: TableFactory.construct(new Structure({offset: new Integer(), length: new Integer()})),
      });

      match.get().line.set((m as any).line || 0);
      match.get().offset.set(m.index);
      match.get().length.set(m[0].length);

      const submatch = new Structure({offset: new Integer(), length: new Integer()});
      for (let i = 1; i < m.length; i++) {
// @ts-ignore
        if (m[i] === undefined) {
          submatch.get().offset.set(-1);
          submatch.get().length.set(0);
        } else {
// @ts-ignore
          submatch.get().offset.set(m.index + m[0].indexOf(m[i]));
// @ts-ignore
          submatch.get().length.set(m[i].length);
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