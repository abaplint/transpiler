import {FieldSymbol} from "./types";

export function expandDynamic(code: string, ev: (name: string) => FieldSymbol | undefined) {
  if (code === "") {
    return "1 = 1";
  } else {
    // yea, this is also wrong,
    code = code.replace(/ EQ /g, " = ");

// todo more here, this is just one simple case,
    let regex = / <(\w+)>-(\w+)/;
    let match = code.match(regex);
    if (match && match[1] && match[2]) {
      const name = "fs_" + match[1] + "_";
      let found = ev(name);
      if (found instanceof FieldSymbol) {
        found = found.get()[match[2]];
        if (found === undefined) {
          throw new Error(`expandDynamic: Field symbol ${name} does not have field ${match[2]}`);
        }
        code = code.replace(regex, " '" + found.get() + "'");
      }
    }

// todo more here, this is just one simple case,
    regex = / <(\w+)>/;
    match = code.match(regex);
    if (match && match[1]) {
      const name = "fs_" + match[1] + "_";
      const found = ev(name);
      if (found instanceof FieldSymbol) {
        code = code.replace(regex, " '" + found.get() + "'");
      }
    }

    return code;
  }
}