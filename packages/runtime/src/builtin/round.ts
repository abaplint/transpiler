import {parse} from "../operators/_parse.js";
import {Float} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";

export function round(input: {val: ICharacter | string, dec: INumeric | number, mode?: INumeric | number}): Float {

  let mode = input.mode;
  if (mode === undefined) {
    mode = 2;
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
    case 2:
      ret.set(Math.round(val));
      break;
    case 4:
      ret.set(-Math.round(-val));
      break;
    case 5:
      ret.set(Math.trunc(val));
      break;
    case 6:
      ret.set(Math.floor(val));
      break;
    default:
      throw "round(), unknown mode: " + mode;
  }

  return ret;
}