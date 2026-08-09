import {throwError} from "../throw_error";
import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {Integer8} from "./integer8";
import {DecFloat34} from "./decfloat34";

const FLOAT_DIGITS = /^\s*[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)? *$/;

/*
function getNumberParts(x: number) {
  if(isNaN(x)) {
    throw new Error("Float NaN";
  }
  const sig = x > 0 ? 1 : -1;
  if (!isFinite(x)) {
    throw new Error("Float not finite";
  }
  x = Math.abs(x);
  const exp = Math.floor(Math.log(x) * Math.LOG2E) - 52;
  const man = x / Math.pow(2, exp);
  return {mantissa: sig * man, exponent: exp};
}
*/

export class Float {
  private value: number;
  private integerCalculationType = false;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.value = 0;
    this.qualifiedName = input?.qualifiedName;
  }

  public clone(): Float {
    const n = new Float({qualifiedName: this.qualifiedName});
    n.value = this.value;
    return n;
  }

  /** ABAP calculates in type i if all operands are integers, ie. "3 / 2" is 2. The exact value is
   * kept here, so assigning to a float or packed target, which raises the calculation type, still
   * gives the exact result. Consumers without a target type must use getCalculationValue() */
  public setIntegerCalculationType(): Float {
    this.integerCalculationType = true;
    return this;
  }

  /** value as seen by the calculation type, ie. rounded if the calculation type is integer */
  public getCalculationValue(): number {
    if (this.integerCalculationType === true) {
      // ABAP rounds half away from zero
      return this.value < 0 ? -Math.round(-this.value) : Math.round(this.value);
    }
    return this.value;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public set(value: INumeric | ICharacter | Hex | string | number) {
    if (typeof value === "number") {
      this.value = value;
    } else if (typeof value === "string" && value.trim().length === 0) {
      this.value = 0;
    } else if (typeof value === "string") {
      if (value.endsWith("-")) {
        value = "-" + value.substring(0, value.length - 1);
      }
      if (FLOAT_DIGITS.test(value) === false) {
        throwError("CX_SY_CONVERSION_NO_NUMBER");
      }
      this.value = parseFloat(value);
    } else if (value instanceof Integer8) {
      this.value = Number(value.get());
    } else if (value instanceof Float || value instanceof DecFloat34) {
      this.value = value.getRaw();
    } else if (value instanceof Hex || value instanceof XString) {
// todo, how/if should this work?
      this.set(parseInt(value.get(), 16));
    } else {
      this.set(value.get());
    }
    return this;
  }

  public clear(): void {
    this.value = 0;
  }

  public getRaw(): number {
    return this.value;
  }

  public get(): string {
    let text = new Number(this.value).toExponential(16);
    text = text.replace(".", ",");
    if (text.includes("e+")) {
      const split = text.split("e+");
      const mantissa = split[0];
      const exponent = split[1].padStart(2, "0");
      return mantissa + "E+" + exponent;
    } else {
      const split = text.split("e-");
      const mantissa = split[0];
      const exponent = split[1].padStart(2, "0");
      return mantissa + "E-" + exponent;
    }
  }
}
