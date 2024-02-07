import {throwError} from "../throw_error";
import {Float} from "./float";
import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export const DIGITS = new RegExp(/^\s*-?\+?\d+\.?\d* *$/i);

export const MAX_INTEGER = 2147483647;
export const MIN_INTEGER = -2147483648;

export function toInteger(value: string, exception = true): number {
  if (value.endsWith("-")) {
    value = "-" + value.substring(0, value.length - 1);
  }
  if (value.trim().length === 0) {
    value = "0";
  } else if (DIGITS.test(value) === false) {
    if (exception === true) {
      throwError("CX_SY_CONVERSION_NO_NUMBER");
    } else {
      throw new Error("CONVT_NO_NUMBER");
    }
  }
  return parseInt(value, 10);
}

export class Integer implements INumeric {
  private value: number;
  private constant: boolean = false;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.value = 0;
    this.qualifiedName = input?.qualifiedName;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public setConstant() {
    this.constant = true;
    return this;
  }

  public set(value: INumeric | ICharacter | Hex | string | number | Integer | Float) {
    if (this.constant === true) {
      throw new Error("Changing constant");
    }

    if (typeof value === "number") {
      this.value = Math.round(value);
    } else if (typeof value === "string") {
      this.value = toInteger(value);
    } else if (value instanceof Float) {
      this.set(Math.round(value.getRaw()));
    } else if (value instanceof Hex || value instanceof XString) {
      let num = parseInt(value.get(), 16);
// handle two complement,
      if (value instanceof Hex && value.getLength() >= 4) {
        const maxVal = Math.pow(2, value.get().length / 2 * 8);
        if (num > maxVal / 2 - 1) {
          num = num - maxVal;
        }
      }
      this.set(num);
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