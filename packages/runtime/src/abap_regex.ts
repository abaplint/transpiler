import {IRegexOptions} from "./statements/find";

// https://github.com/TooTallNate/pcre-to-regexp/blob/master/src/index.ts
export class ABAPRegExp {
  // converts from ABAP specific regex to javascript regex
  public static convert(input: string): string {
    let ret = input;

    ret = ret.replace(/\[\[:punct:\]\]/g, "[@%\\.\\,\\-\\{\\}\\[\\]\\:\\!\\?\\(\\)\\;\\']");

    // https://github.com/micromatch/posix-character-classes#posix-character-classes
    ret = ret.replace(/\[\^\[:print:\]\]/g, "[\\x00-\\x1F\\x7F]");

    // https://github.com/micromatch/posix-character-classes#posix-character-classes
    ret = ret.replace(/\[\[:digit:\]\]/g, "[0-9]");

    // https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenregex_syntax_specials.htm
    ret = ret.replace(/\\C/g, "[a-zA-Z]");

    ret = ret.replace("[[:space:]]", "\\s");

    return ret;
  }

  public static escapeRegExp(text: string) {
    return text.replace(/[-[\]{}()*+?.,\\^$|#]/g, "\\$&");
  }

  public static getRegex(options: IRegexOptions): RegExp {
    if (options.regex) {
      if (typeof options.regex === "string") {
        if (options.regex === "") {
          throw new Error("FIND, runtime, no input, regex empty");
        }
      } else if (options.regex.get() === "") {
        throw new Error("FIND, runtime, no input, regex empty");
      }
    }

    let ignoreCase = options.ignoringCase === true ? "i" : "";
    const allOccurrences = options.all === true ? "g" : "";

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
      // TODO: tackle the "mv_" variables properly, this is a workaround
      // @ts-ignore
      r = obj.mv_pattern.get();
      // @ts-ignore
      if (obj.mv_ignore_case.get() === "X") {
        ignoreCase = 'i';
      }
    } else {
      throw "getRegex(), unexpected input";
    }

    r = r as string;

    if (r.length === 0 && options.all === true) {
      throw "getRegex(), zero length input";
    }

    return new RegExp(r, "m" + ignoreCase + allOccurrences);
  }
}
