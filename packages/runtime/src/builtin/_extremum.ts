import {parse} from "../operators/_parse";
import {DecFloat34, FieldSymbol, Float, Integer, Integer8, Packed} from "../types";
import {INumeric} from "../types/_numeric";

export type ExtremumArgument = number | INumeric | undefined;

// the calculation types in the order an arithmetic expression ranks them
const INT = 0;
const INT8 = 1;
const PACKED = 2;
const FLOAT = 3;
const DECFLOAT = 4;

/** nmax( ) and nmin( ): the result has the calculation type of the arguments,
 * as an arithmetic expression would determine it - decfloat34 before f before
 * p before int8 before i. So nmin( val1 = i1 val2 = i2 ) is an i, which DO
 * nmin( ... ) TIMES and every other integer position rely on, and a p result
 * keeps the most decimals of its arguments. */
export function extremum(args: ExtremumArgument[], isBetter: (candidate: number, best: number) => boolean) {
  let best: number | INumeric | undefined = undefined;
  let bestValue = 0;
  let type = INT;
  let decimals = 0;

  for (let arg of args) {
    if (arg === undefined) {
      continue;
    }
    if (arg instanceof FieldSymbol) {
      arg = arg.getPointer() as INumeric;
    }

    if (arg instanceof DecFloat34) {
      type = Math.max(type, DECFLOAT);
    } else if (arg instanceof Float) {
      type = Math.max(type, FLOAT);
    } else if (arg instanceof Packed) {
      type = Math.max(type, PACKED);
      decimals = Math.max(decimals, arg.getDecimals());
    } else if (arg instanceof Integer8) {
      type = Math.max(type, INT8);
    } else if (arg instanceof Integer || (typeof arg === "number" && Number.isInteger(arg))) {
      // i, the lowest type
    } else {
      // character-like and other arguments, as before: f
      type = Math.max(type, FLOAT);
    }

    const value = parse(arg as any);
    if (best === undefined || isBetter(value, bestValue)) {
      best = arg;
      bestValue = value;
    }
  }

  switch (type) {
    case DECFLOAT:
      return new DecFloat34().set(bestValue);
    case FLOAT:
      return new Float().set(bestValue);
    case PACKED:
      if (best instanceof Packed) {
        // through the digits, not through a binary float
        return new Packed({length: 16, decimals}).set(best.toFixed(best.getDecimals()));
      }
      return new Packed({length: 16, decimals}).set(bestValue);
    case INT8:
      if (best instanceof Integer8) {
        return new Integer8().set(best.get());
      }
      return new Integer8().set(bestValue);
    default:
      return new Integer().set(bestValue);
  }
}
