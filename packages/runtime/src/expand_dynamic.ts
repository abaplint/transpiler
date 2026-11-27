import {expandIN} from "./expand_in";
import {sqlEscape} from "./sql_escape";
import {FieldSymbol, HashedTable, Table} from "./types";

type DynamicCondition = string | Table | HashedTable | {get(): string};

export function expandDynamic(input: DynamicCondition, evaluate: (name: string) => FieldSymbol | undefined) {
//  console.dir(code);
  let code: string;
  if (input instanceof Table || input instanceof HashedTable) {
    code = input.array().map(row => row.get()).join(" ");
  } else if (typeof input === "string") {
    code = input;
  } else {
    code = input.get();
  }

  if (code === "") {
    return "1 = 1";
  } else {
    // yea, this is also wrong,
    code = code.replace(/ EQ /g, " = ");

// todo more here, these are just the simple cases.
// One pass over the text: a value put in is never scanned again, so a value
// holding " <x>" is data, and the replacement is a function so that "$'",
// "$&" and the like in a value are not read as replacement patterns.
    const regex = /(?:(\w+) IN)? @?<(\w+)>(?:-(\w+))?/g;
    code = code.replace(regex, (whole: string, field: string | undefined, fs: string, component: string | undefined) => {
      const name = ("fs_" + fs + "_").toLowerCase();
      const found = evaluate(name);
      if (!(found instanceof FieldSymbol)) {
        return whole;
      }
      let value: any = found;
      if (component !== undefined) {
        value = found.get()[component.toLowerCase()];
        if (value === undefined) {
          throw new Error(`expandDynamic: Field symbol ${name} does not have field ${component}`);
        }
        if (field !== undefined) {
          return expandIN(field.toLowerCase(), value);
        }
      }
      const prefix = field !== undefined ? field + " IN" : "";
      return prefix + " '" + sqlEscape(value.get()) + "'";
    });

//    console.dir(code);
    return code;
  }
}
