// https://github.com/TooTallNate/pcre-to-regexp/blob/master/src/index.ts
export class ABAPRegExp {
  // converts from ABAP specific regex to javascript regex
  public static convert(input: string): string {
    let ret = input;

    ret = ret.replace(/\[\[:punct:\]\]/g, "[@%\\.\\,\\-\\{\\}\\[\\]\\:\\!\\?\\(\\)\\;\\']");

    // https://github.com/micromatch/posix-character-classes#posix-character-classes
    ret = ret.replace(/\[\^\[:print:\]\]/g, "[\\x00-\\x1F\\x7F]");

    ret = ret.replace("[[:space:]]", "\\s");

    return ret;
  }

  public static escapeRegExp(text: string) {
    return text.replace(/[-[\]{}()*+?.,\\^$|#]/g, "\\$&");
  }
}
