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
  return Math.round(parseFloat(value));
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

  public set(value: INumeric | ICharacter | Hex | string | number | Integer | Float | DecFloat34) {
    if (this.constant === true) {
      throw new Error("Changing constant");
    }

    if (typeof value === "number") {
      this.value = Math.round(value);
    } else if (value instanceof Integer) {
      this.set(value.get());
    } else if (value instanceof Character) {
      this.value = toInteger(value.get());
    } else if (value instanceof String) {
      this.value = toInteger(value.get());
    } else if (value instanceof Integer8) {
      this.set(Number(value.get()));
    } else if (value instanceof Float || value instanceof DecFloat34) {
      this.set(Math.round(value.getRaw()));
    } else if (value instanceof Hex || value instanceof XString || value instanceof HexUInt8) {
      const hex = value.get();
      let num = parseInt(hex, 16);
// handle two complement, the first bit is the sign
      if (hex.length >= 8) {
        const maxVal = Math.pow(2, hex.length / 2 * 8);
        if (num > maxVal / 2 - 1) {
          num = num - maxVal;
        }
      }
      this.set(num);
    } else if (typeof value === "string") {
      this.value = toInteger(value);
    } else {
      this.set(value.get());
    }
/*
    if (this.value > 2147483647 || this.value < -2147483648) {
      throwError("CX_SY_ARITHMETIC_OVERFLOW");
    }
*/
    return this;
  }

  public clear(): void {
    this.value = 0;
  }

  public get(): number {
    return this.value;
  }
}
