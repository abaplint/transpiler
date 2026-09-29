import {parse} from "../operators/_parse";
import {DecFloat34, FieldSymbol, Float} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

// the rounding modes of CL_ABAP_MATH, the decNumber enumeration
const ROUND_CEILING = 0;
const ROUND_UP = 1;
const ROUND_HALF_UP = 2;
const ROUND_HALF_EVEN = 3;
const ROUND_HALF_DOWN = 4;
const ROUND_DOWN = 5;
const ROUND_FLOOR = 6;

type Input = INumeric | ICharacter | string | number;

/** round( val = arg {dec = n}|{prec = n} [mode = m] )
 *
 * The argument is converted to decfloat34 and rounded as a decimal number, so
 * round( val = '2.675' dec = 2 ) is 2.68 - not 2.67, which is what scaling the
 * nearest binary float would give. A binary float argument brings its full
 * binary expansion along, the way converting f to decfloat34 does. */
export function round(input: {val: Input, dec?: Input, prec?: Input, mode?: Input}): DecFloat34 {
  const mode = input.mode === undefined ? ROUND_HALF_UP : parse(input.mode as any);

  let arg = input.val;
  if (arg instanceof FieldSymbol) {
    arg = arg.getPointer();
  }
  const val = parse(arg as any);
  const text = arg instanceof Float ? val.toPrecision(34) : String(val);

  const match = /^(-?)(\d*)\.?(\d*)(?:e([+-]?\d+))?$/i.exec(text);
  if (match === null || !Number.isFinite(val)) {
    return new DecFloat34().set(val);
  }
  const negative = match[1] === "-";
  const digits = BigInt((match[2] + match[3]).replace(/^0+/, "") || "0");
  // the value is digits * 10^exponent
  const exponent = parseInt(match[4] || "0", 10) - match[3].length;

  let dec: number;
  if (input.prec !== undefined) {
    const prec = parse(input.prec as any);
    if (prec <= 0) {
      throw new Error("CX_SY_ARITHMETIC_ERROR, round( ) prec must be positive");
    }
    // position of the most significant digit
    const top = digits.toString().length - 1 + exponent;
    dec = prec - 1 - top;
  } else {
    dec = input.dec === undefined ? 0 : parse(input.dec as any);
  }

  const drop = -exponent - dec;
  if (drop <= 0 || digits === 0n) {
    return new DecFloat34().set(val);
  }

  const divisor = 10n ** BigInt(drop);
  let kept = digits / divisor;
  const rest = digits % divisor;
  // compare the dropped part with one half of the last kept digit
  const half = rest * 2n === divisor ? 0 : (rest * 2n > divisor ? 1 : -1);

  let away: boolean;
  switch (mode) {
    case ROUND_CEILING:
      away = rest > 0n && negative === false;
      break;
    case ROUND_UP:
      away = rest > 0n;
      break;
    case ROUND_HALF_UP:
      away = half >= 0;
      break;
    case ROUND_HALF_EVEN:
      away = half > 0 || (half === 0 && kept % 2n === 1n);
      break;
    case ROUND_HALF_DOWN:
      away = half > 0;
      break;
    case ROUND_DOWN:
      away = false;
      break;
    case ROUND_FLOOR:
      away = rest > 0n && negative === true;
      break;
    default:
      throw new Error("round(), unknown mode: " + mode);
  }
  if (away) {
    kept += 1n;
  }

  return new DecFloat34().set(Number((negative ? "-" : "") + kept.toString() + "e" + (-dec)));
}
