import {Float} from "./float";
import {INumeric} from "./_numeric";
import {throwError} from "../throw_error";
import {Integer8} from "./integer8";

const digits = new RegExp(/^\s*-?\+?\d*\.?\d*(?:E[+-]?\d+)? *$/i);

function pow10(exp: number): bigint {
  return 10n ** BigInt(exp);
}

// rescale a non-negative magnitude from fromScale to toScale decimal places,
// rounding half away from zero
function rescale(mag: bigint, fromScale: number, toScale: number): bigint {
  if (toScale >= fromScale) {
    return mag * pow10(toScale - fromScale);
  }
  const div = pow10(fromScale - toScale);
  const q = mag / div;
  const r = mag % div;
  return r * 2n >= div ? q + 1n : q;
}

// format a non-negative magnitude at the given scale as a decimal string
function formatMag(mag: bigint, scale: number): string {
  let s = mag.toString();
  if (scale === 0) {
    return s;
  }
  while (s.length <= scale) {
    s = "0" + s;
  }
  const intPart = s.slice(0, s.length - scale);
  const fracPart = s.slice(s.length - scale);
  return intPart + "." + fracPart;
}

export class Packed implements INumeric {
  // value holds the number scaled by 10^decimals, as a bigint, to keep full precision
  private value: bigint;
  private readonly length: number;
  private readonly decimals: number;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {length?: number, decimals?: number, qualifiedName?: string}) {
    this.value = 0n;

    this.length = 666;
    if (input?.length) {
      this.length = input.length;
    }

    this.decimals = 0;
    if (input?.decimals) {
      this.decimals = input.decimals;
    }

    this.qualifiedName = input?.qualifiedName;
  }

  public clone(): Packed {
    const n = new Packed({length: this.length, decimals: this.decimals, qualifiedName: this.qualifiedName});
    n.value = this.value;
    return n;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  private numberToScaled(value: number): bigint {
    const factor = Math.pow(10, this.decimals);
    return BigInt(Math.round(value * factor));
  }

  private stringToScaled(input: string): bigint {
    let str = input.trim();

    let negative = false;
    while (str.length > 0 && (str[0] === "+" || str[0] === "-")) {
      if (str[0] === "-") {
        negative = true;
      }
      str = str.slice(1);
    }

    if (/[eE]/.test(str)) {
      // scientific notation, fall back to floating point parsing
      return this.numberToScaled(parseFloat((negative ? "-" : "") + str));
    }

    let intPart = str;
    let fracPart = "";
    const dot = str.indexOf(".");
    if (dot >= 0) {
      intPart = str.slice(0, dot);
      fracPart = str.slice(dot + 1);
    }
    if (intPart === "") {
      intPart = "0";
    }

    const kept = fracPart.slice(0, this.decimals).padEnd(this.decimals, "0");
    let scaled = BigInt(intPart + kept);
    if (fracPart.length > this.decimals && (fracPart.charCodeAt(this.decimals) - 48) >= 5) {
      scaled += 1n;
    }

    return negative ? -scaled : scaled;
  }

  public set(value: INumeric | number | string) {
    if (typeof value === "number") {
      this.value = this.numberToScaled(value);
    } else if (typeof value === "string") {
      if (value.trim().length === 0) {
        this.value = 0n;
        return this;
      } else if (digits.test(value) === false) {
        throwError("CX_SY_CONVERSION_NO_NUMBER");
      }

      this.value = this.stringToScaled(value);
    } else if (value instanceof Integer8) {
      this.value = (value.get() as unknown as bigint) * pow10(this.decimals);
    } else if (value instanceof Float) {
      this.value = this.numberToScaled(value.getRaw());
    } else {
      this.set(value.get());
    }
    return this;
  }

  public getLength() {
    return this.length;
  }

  public getDecimals() {
    return this.decimals;
  }

  // returns the value as a fixed decimal string with the given number of
  // decimals, keeping full precision (unlike get() which is limited to double)
  public toFixed(decimals: number): string {
    const negative = this.value < 0n;
    const mag = negative ? -this.value : this.value;
    const scaled = rescale(mag, this.decimals, decimals);
    const formatted = formatMag(scaled, decimals);
    return (negative ? "-" : "") + formatted;
  }

  public clear(): void {
    this.value = 0n;
  }

  public get(): number {
    return Number(this.value) / Math.pow(10, this.decimals);
  }
}
