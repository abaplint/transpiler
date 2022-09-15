import {parse} from "../operators/_parse";
import {Float} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function round(input: {val: ICharacter | string, dec: INumeric | number, mode?: INumeric | number}): Float {

  let mode = input.mode;
  if (mode === undefined) {
    mode = 1;
  } else if (typeof mode !== "number") {
    mode = mode?.get();
  }

  const val = parse(input.val);
  const dec = parse(input.dec);
  if (dec !== 0) {
    throw "round(), todo, handle decimals";
  }

  const ret = new Float();
  switch (mode) {
    case 1:
      ret.set(Math.ceil(val));
      break;
    case 4:
      ret.set(-Math.round(-val));
      break;
    case 6:
      ret.set(Math.floor(val));
      break;
    default:
      throw "round(), unknown mode: " + mode;
  }

  return ret;
}