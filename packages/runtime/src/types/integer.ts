import {throwError} from "../throw_error";
import {Float} from "./float";
import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {Integer8} from "./integer8";
import {HexUInt8} from "./hex_uint8";
import {Character} from "./character";
import {String} from "./string";
import {DecFloat34} from "./decfloat34";
import {Packed} from "./packed";

export const DIGITS = new RegExp(/^\s*-?\+?\d+\.?\d* *$/i);

export const MAX_INTEGER = 2147483647;
export const MIN_INTEGER = -2147483648;

export function toInteger(value: string, exception = true): number {
  if (value.endsWith("-")) {
    value = "-" + value.substring(0, value.length - 1);
  }

  if (DIGITS.test(value) === false) {
    if (value.trim().length === 0) {
      value = "0";
    } else if (exception === true) {
      throwError("CX_SY_CONVERSION_NO_NUMBER");
    } else {
      throw new Error("CONVT_NO_NUMBER");
    }
  }
  return roundHalfAwayFromZero(parseFloat(value));
}

// ABAP rounds a half away from zero when a float is moved to an integer,
// on both sides: 0.5 is 1 and -0.5 is -1. Math.round rounds a half towards
// positive infinity, so -0.5 was 0 (and -0 at that)
export function roundHalfAwayFromZero(value: number): number {
  const rounded = value < 0 ? -Math.round(-value) : Math.round(value);
  return rounded === 0 ? 0 : rounded;
}

/** a value converted to type i, CX_SY_CONVERSION_OVERFLOW if it is out of range */
function checkConversion(value: number): number {
  if (value > MAX_INTEGER || value < MIN_INTEGER) {
    throwError("CX_SY_CONVERSION_OVERFLOW");
  }
  return value;
}

export class Integer implements INumeric {
  private value: number;
  private constant: boolean = false;
  private integerCalculationType = true;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.value = 0;
    this.qualifiedName = input?.qualifiedName;
  }

  /** ABAP determines the calculation type from the operand types, character-like operands raise it
   * above type i even if the intermediate result is integer, ie. "'1.0' * 18 / 16" is not rounded */
  public clearIntegerCalculationType(): Integer {
    this.integerCalculationType = false;
    return this;
  }

  public isIntegerCalculationType(): boolean {
    return this.integerCalculationType;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public clone(): Integer {
    const n = new Integer({qualifiedName: this.qualifiedName});
    // set without trigger checks and padding
    n.value = this.value;
    return n;
  }

  public setConstant() {
    this.constant = true;
    return this;
  }

  /** the result of integer arithmetic, without a range check: the calculation type is decided by
   * the target, a p or int8 target takes 2147483647 + 1 as it is, and an i target raises
   * CX_SY_ARITHMETIC_OVERFLOW when the result is assigned to it, see set() */
  public static calculated(value: number): Integer {
    const n = new Integer();
    n.value = value;
    return n;
  }

  public set(value: INumeric | ICharacter | Hex | string | number | Integer | Float | DecFloat34) {
    if (this.constant === true) {
      throw new Error("Changing constant");
    }

    if (typeof value === "number") {
      // a number is the result of a calculation or a built-in function. A whole number in range
      // is the common case and is taken as it is, + 0 turns -0 into 0
      if (Number.isInteger(value) && value <= MAX_INTEGER && value >= MIN_INTEGER) {
        this.value = value + 0;
        return this;
      }
      const v = roundHalfAwayFromZero(value);
      if (v > MAX_INTEGER || v < MIN_INTEGER) {
        throwError("CX_SY_ARITHMETIC_OVERFLOW");
      }
      this.value = v;
      return this;
    } else if (value instanceof Integer) {
      // a variable of type i is always in range, so a value outside it is an arithmetic result
      const v = value.value;
      if (v > MAX_INTEGER || v < MIN_INTEGER) {
        throwError("CX_SY_ARITHMETIC_OVERFLOW");
      }
      this.value = v;
      return this;
    }
    return this.convert(value);
  }

  /** set() for everything but the two common cases, kept apart so that set() stays small */
  private convert(value: INumeric | ICharacter | Hex | string | Float | DecFloat34): Integer {
    if (value instanceof Character) {
      this.value = checkConversion(toInteger(value.get()));
    } else if (value instanceof String) {
      this.value = checkConversion(toInteger(value.get()));
    } else if (value instanceof Integer8) {
      const v: bigint = value.get() as bigint;
      if (v > 2147483647n || v < -2147483648n) {
        throwError("CX_SY_CONVERSION_OVERFLOW");
      }
      this.value = Number(v);
    } else if (value instanceof Float) {
      // rounding comes first, 2147483647.4 fits and 2147483647.5 does not
      const v = roundHalfAwayFromZero(value.getRaw());
      if (v > MAX_INTEGER || v < MIN_INTEGER) {
        // the result of an arithmetic expression overflows, a variable does not convert
        throwError(value.isCalculated() ? "CX_SY_ARITHMETIC_OVERFLOW" : "CX_SY_CONVERSION_OVERFLOW");
      }
      this.value = v;
    } else if (value instanceof DecFloat34 || value instanceof Packed) {
      this.value = checkConversion(roundHalfAwayFromZero(value instanceof Packed ? value.get() : value.getRaw()));
    } else if (value instanceof Hex || value instanceof XString || value instanceof HexUInt8) {
// the last four bytes, 00 on the left, as a signed integer; an empty xstring is 0
      const hex = value.get().slice(-8);
      let num = hex === "" ? 0 : parseInt(hex, 16);
      if (hex.length === 8 && num > 0x7FFFFFFF) {
        num = num - 0x100000000;
      }
      this.value = num;
    } else if (typeof value === "string") {
      this.value = checkConversion(toInteger(value));
    } else {
      const v = value.get();
      if (typeof v === "number") {
        this.value = checkConversion(roundHalfAwayFromZero(v));
      } else if (typeof v === "string") {
        this.value = checkConversion(toInteger(v));
      } else {
        this.set(v);
      }
    }
    return this;
  }

  public clear(): void {
    this.value = 0;
  }

  public get(): number {
    return this.value;
  }
}
