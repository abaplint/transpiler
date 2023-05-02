import {FieldSymbol} from "./types/field_symbol.js";

export function expandDynamic(code: string, ev: (name: string) => FieldSymbol | undefined) {
  if (code === "") {
    return "1 = 1";
  } else {
// todo more here, this is just one simple case,
    const match = code.match(/ <(\w+)>/);
    if (match && match[1]) {
      const name = "fs_" + match[1] + "_";
      const found = ev(name);
      if (found instanceof FieldSymbol) {
        code = code.replace(/ <(\w+)>/, "'" + found.get() + "'");
      }
    }
    return code;
  }
}