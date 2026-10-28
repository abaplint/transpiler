import {IRegexOptions} from "./statements/find";
import {throwError} from "./throw_error";

// https://github.com/TooTallNate/pcre-to-regexp/blob/master/src/index.ts
export class ABAPRegExp {
  // the POSIX character classes ABAP takes inside a bracket expression, and what the same
  // characters are in a JavaScript class; print keeps the ASCII range the [^[:print:]] rule
  // below has always answered
  private static readonly POSIX_CLASSES: {[name: string]: string} = {
    "alpha": "a-zA-Z",
    "alnum": "a-zA-Z0-9",
    "upper": "A-Z",
    "lower": "a-z",
    "digit": "0-9",
    "xdigit": "0-9A-Fa-f",
    "space": "\\s",
    "blank": " \\t",
    "cntrl": "\\x00-\\x1F\\x7F",
    "print": "\\x20-\\x7E",
    "graph": "\\x21-\\x7E",
    "punct": "@%\\.\\,\\-\\{\\}\\[\\]\\:\\!\\?\\(\\)\\;\\'",
    "word": "\\w",
  };

  /** every [:name:] inside a bracket expression becomes the JavaScript characters of that
   * class, so [[:alpha:]], [^[:alpha:][:digit:]] and [[:alpha:]_] all work; an unknown name
   * raises CX_SY_REGEX like a system does */
  private static convertPosixClasses(input: string): string {
    let ret = "";
    let inClass = false;
    for (let i = 0; i < input.length; i++) {
      const c = input[i];
      if (c === "\\" && i + 1 < input.length) {
        ret += c + input[i + 1];
        i++;
      } else if (inClass && c === "[" && input[i + 1] === ":") {
        const end = input.indexOf(":]", i + 2);
        const name = end === -1 ? undefined : input.substring(i + 2, end);
        if (name === undefined || /^[a-z]+$/.test(name) === false) {
          ret += c;
          continue;
        }
        const chars = ABAPRegExp.POSIX_CLASSES[name];
        if (chars === undefined) {
          throwError("CX_SY_REGEX");
        }
        ret += chars;
        i = end + 1;
      } else if (inClass) {
        ret += c;
        if (c === "]") {
          inClass = false;
        }
      } else if (c === "[") {
        inClass = true;
        ret += c;
        // a ] right after [ or [^ is a character, not the end of the class
        if (input[i + 1] === "^") {
          ret += "^";
          i++;
        }
        if (input[i + 1] === "]") {
          ret += "]";
          i++;
        }
      } else {
        ret += c;
      }
    }
    return ret;
  }

  // converts from ABAP specific regex to javascript regex
  public static convert(input: string): string {
    let ret = input;

    // https://github.com/micromatch/posix-character-classes#posix-character-classes
    ret = ret.replace(/\[\^\[:print:\]\]/g, "[\\x00-\\x1F\\x7F]");

    ret = ABAPRegExp.convertPosixClasses(ret);

    // https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenregex_syntax_specials.htm
    ret = ret.replace(/\\C/g, "[a-zA-Z]");

    ret = ret.replace("[[:space:]]", "\\s");

    // buffer anchors, note that the "m" flag is set, so ^ and $ cannot be used
    // https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenregex_search.htm
    ret = ret.replace(/\\[\s\S]/g, (m) => {
      switch (m) {
        case "\\A":
          return "(?<![\\s\\S])";
        case "\\z":
          return "(?![\\s\\S])";
        case "\\Z":
          return "(?=\\n*(?![\\s\\S]))";
        default:
          return m;
      }
    });

    return ret;
  }

  /** A JavaScript regular expression for an ABAP pattern; Unicode properties like \p{L} need the u flag */
  public static create(pattern: string, flags: string): RegExp {
    return new RegExp(pattern, /\\[pP]\{/.test(pattern) ? flags + "u" : flags);
  }

  /** The replacement text of a regex replacement in JavaScript notation: $0 and $& are the match, $1 to $9
   * a submatch, a backslash takes the next character literally, and any other $ is a dollar sign */
  public static convertReplacement(input: string): string {
    let ret = "";
    for (let i = 0; i < input.length; i++) {
      const c = input[i];
      const next = input[i + 1];
      if (c === "\\" && next !== undefined) {
        ret += next === "$" ? "$$" : next;
        i++;
      } else if (c === "$" && (next === "0" || next === "&")) {
        ret += "$&";
        i++;
      } else if (c === "$" && next !== undefined && "123456789`'".includes(next)) {
        ret += c + next;
        i++;
      } else if (c === "$" && next === "{" && /^\{\d+\}/.test(input.substring(i + 1))) {
        const digits = /^\{(\d+)\}/.exec(input.substring(i + 1))![1];
        ret += digits === "0" ? "$&" : "$" + digits;
        i += digits.length + 2;
      } else if (c === "$") {
        ret += "$$";
      } else {
        ret += c;
      }
    }
    return ret;
  }

  /** Extended mode: white space outside a character class is ignored and # starts a comment to the end of the line */
  public static removeExtended(pattern: string): string {
    let ret = "";
    let inClass = false;
    for (let i = 0; i < pattern.length; i++) {
      const c = pattern[i];
      if (c === "\\" && i + 1 < pattern.length) {
        ret += c + pattern[i + 1];
        i++;
      } else if (inClass) {
        ret += c;
        if (c === "]") {
          inClass = false;
        }
      } else if (c === "[") {
        inClass = true;
        ret += c;
        // a ] right after [ or [^ is a character, not the end of the class
        if (pattern[i + 1] === "^") {
          ret += "^";
          i++;
        }
        if (pattern[i + 1] === "]") {
          ret += "]";
          i++;
        }
      } else if (c === "#") {
        while (i + 1 < pattern.length && pattern[i + 1] !== "\n") {
          i++;
        }
      } else if (/\s/.test(c) === false) {
        ret += c;
      }
    }
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
    let multiLine = "m";
    let dotAll = "";
    let extended = false;
    const allOccurrences = options.all === true ? "g" : "";

    let r = options.regex!;
    if (r === undefined) {
      r = options.pcre!;
    }
    if (typeof r !== "string") {
      r = r.get();
    }

    if (r.constructor.name === "cl_abap_regex") {
      const obj = r;
      // TODO: tackle the "mv_" variables properly, this is a workaround
      // @ts-ignore
      r = obj.mv_pattern.get() as string;
      // @ts-ignore
      if (obj.mv_ignore_case.get() === "X") {
        ignoreCase = 'i';
      }
      // a regex from create_pcre( ) carries its options: the anchors match at line breaks only with
      // enable_multiline, the dot matches a line break only with dot_all, and extended is the default
      // @ts-ignore
      if (obj.mv_pcre?.get() === "X") {
        // @ts-ignore
        multiLine = obj.mv_multiline?.get() === "X" ? "m" : "";
        // @ts-ignore
        dotAll = obj.mv_dot_all?.get() === "X" ? "s" : "";
        // @ts-ignore
        extended = obj.mv_extended?.get() === "X";
      }
    } else if (typeof r !== "string") {
      throw new Error("getRegex(), unexpected input");
    }

    r = ABAPRegExp.convert(r);
    if (extended === true) {
      r = ABAPRegExp.removeExtended(r);
    }

    if (r.length === 0 && options.all === true) {
      throw new Error("getRegex(), zero length input");
    }

    return ABAPRegExp.create(r, multiLine + dotAll + ignoreCase + allOccurrences);
  }
}
