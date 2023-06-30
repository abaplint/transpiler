import {xstrlen} from "../builtin";
import {Hex, XString} from "../types";

// note: javascript bitwise negate doesnt work well for long hex values
export function bitnot(right: XString | Hex) {
  const len = xstrlen({val: right}).get();
  let not = "";

  for (let i = 0; i < len; i++) {
    const byte = right.getOffset({offset: i, length: 1});

    let binary = parseInt(byte.get(), 16).toString(2).padStart(8, "0");
    binary = binary.replace(/0/g, "2");
    binary = binary.replace(/1/g, "0");
    binary = binary.replace(/2/g, "1");

    const hex = parseInt(binary, 2).toString(16).padStart(2, "0");
    not += hex;
  }

  const ret = new Hex({length: right.get().length / 2});
  ret.set(not.toUpperCase());

  return ret;
}