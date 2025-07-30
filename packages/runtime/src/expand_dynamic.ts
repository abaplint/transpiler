import {expandIN} from "./expand_in";
import {FieldSymbol} from "./types";

export function expandDynamic(code: string, ev: (name: string) => FieldSymbol | undefined) {
//  console.dir(code);
  if (code === "") {
    return "1 = 1";
  } else {
    // yea, this is also wrong,
    code = code.replace(/ EQ /g, " = ");

// todo more here, this is just one simple case,
    let regex = /(\w+) IN <(\w+)>-(\w+)/;
    while(true) {
      const match = code.match(regex);
      if (match && match[1] && match[2] && match[3]) {
        let name = "fs_" + match[2] + "_";
        name = name.toLowerCase();
        let found = ev(name);
        if (found instanceof FieldSymbol) {
          found = found.get()[match[3].toLowerCase()];
          if (found === undefined) {
            throw new Error(`expandDynamic: Field symbol ${name} does not have field ${match[2]}`);
          }
          code = code.replace(regex, expandIN(match[1].toLowerCase(), found as any));
        }
      } else {
        break;
      }
    }

// todo more here, this is just one simple case,
    regex = / <(\w+)>-(\w+)/;
    while(true) {
      const match = code.match(regex);
      if (match && match[1] && match[2]) {
        let name = "fs_" + match[1] + "_";
        name = name.toLowerCase();
        let found = ev(name);
        if (found instanceof FieldSymbol) {
          found = found.get()[match[2].toLowerCase()];
          if (found === undefined) {
            throw new Error(`expandDynamic: Field symbol ${name} does not have field ${match[2]}`);
          }
          code = code.replace(regex, " '" + found.get() + "'");
        }
      } else {
        break;
      }
    }

// todo more here, this is just one simple case,
    regex = / <(\w+)>/;
    while(true) {
      const match = code.match(regex);
      if (match && match[1]) {
        let name = "fs_" + match[1] + "_";
        name = name.toLowerCase();
        const found = ev(name);
        if (found instanceof FieldSymbol) {
          code = code.replace(regex, " '" + found.get() + "'");
        }
      } else {
        break;
      }
    }

//    console.dir(code);
    return code;
  }
}