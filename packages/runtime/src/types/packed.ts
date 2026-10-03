import {Float} from "./float";
import {INumeric} from "./_numeric";
import {throwError} from "../throw_error";
import {Integer8} from "./integer8";
import {DecFloat34} from "./decfloat34";

const digits = new RegExp(/^\s*-?\+?\d*\.?\d*(?:E[+-]?\d+)? *$/i);

// Exact Number powers used only for safe integer scaling.
const NUMBER_POW10 = [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000,
  100000000, 1000000000, 10000000000, 100000000000, 1000000000000,
  10000000000000, 100000000000000, 1000000000000000];

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

// rescale a signed value, rounding half away from zero
function rescaleSigned(value: bigint, fromScale: number, toScale: number): bigint {
  if (fromScale === toScale) {
    return value;
  } else if (value < 0n) {
    return -rescale(-value, fromScale, toScale);
  }
  return rescale(value, fromScale, toScale);
}

// Keep the representation canonical so unsafe values skip the Number path
// without a bigint-to-Number conversion at every operator and assignment.
function compact(value: bigint): bigint | number {
  const n = Number(value);
  return Number.isSafeInteger(n) ? n : value;
}

// Only use Number when the integer intermediates are exact. Remainder-based
// rounding avoids a floating quotient rounding up just below a half boundary.
function rescaleSafe(value: number, fromScale: number, toScale: number): number | undefined {
  if (fromScale === toScale) {
    return value;
  }
  const difference = Math.abs(toScale - fromScale);
  if (difference > 15) {
    return undefined;
  }
  const factor = NUMBER_POW10[difference];
  if (toScale > fromScale) {
    const scaled = value * factor;
    return Number.isSafeInteger(scaled) ? scaled : undefined;
  }
  const mag = Math.abs(value);
  const rem = mag % factor;
  const rounded = (mag - rem) / factor + (rem >= factor / 2 ? 1 : 0);
  return value < 0 && rounded !== 0 ? -rounded : rounded;
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
  // Exact integer scaled by 10^decimals; Number for safe integers, bigint otherwise.
  private value: bigint | number;
  private readonly length: number;
  private readonly decimals: number;
  private readonly qualifiedName: string | undefined;

  // fromScaled supplies the calculation scale directly to avoid an options object.
  public constructor(input?: {length?: number, decimals?: number, qualifiedName?: string}, calculationDecimals?: number) {
    this.value = 0;

    this.length = calculationDecimals === undefined ? 666 : 16;
    if (input?.length) {
      this.length = input.length;
    }

    this.decimals = calculationDecimals ?? 0;
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

  /** A calculation result scaled by 10^decimals. Number arguments must be safe integers. */
  public static fromScaled(value: bigint | number, decimals: number): Packed {
    const n = new Packed(undefined, decimals);
    n.value = typeof value === "bigint" ? compact(value) : value === 0 ? 0 : value;
    return n;
  }

  /** the value scaled by 10^decimals, exact */
  public getScaled(): bigint {
    return typeof this.value === "number" ? BigInt(this.value) : this.value;
  }

  /** Safe scaled integer for arithmetic without bigint intermediates. */
  public getSafeScaled(): number | undefined {
    return typeof this.value === "number" ? this.value : undefined;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  private numberToScaled(value: number): bigint | number {
    const magnitude = Math.round(Math.abs(value) * Math.pow(10, this.decimals));
    const scaled = value < 0 ? -magnitude : magnitude;
    return scaled === 0 ? 0 : Number.isSafeInteger(scaled) ? scaled : BigInt(scaled);
  }

  private stringToScaled(input: string): bigint | number {
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
    // round half up based on the first dropped fractional digit
    if (fracPart.length > this.decimals && fracPart[this.decimals] >= "5") {
      scaled += 1n;
    }

    return compact(negative ? -scaled : scaled);
  }

  public set(value: INumeric | number | string) {
    if (typeof value === "number") {
      this.value = this.numberToScaled(value);
    } else if (typeof value === "string") {
      if (value.trim().length === 0) {
        this.value = 0;
        return this;
      } else if (digits.test(value) === false) {
        throwError("CX_SY_CONVERSION_NO_NUMBER");
      }

      this.value = this.stringToScaled(value);
    } else if (value instanceof Packed) {
      if (value.decimals === this.decimals) {
        this.value = value.value;
      } else {
        const safe = value.getSafeScaled();
        const scaled = safe === undefined ? undefined : rescaleSafe(safe, value.decimals, this.decimals);
        this.value = scaled === undefined ? compact(rescaleSigned(value.getScaled(), value.decimals, this.decimals)) : scaled;
      }
    } else if (value instanceof Integer8) {
      this.value = compact((value.get() as unknown as bigint) * pow10(this.decimals));
    } else if (value instanceof Float || value instanceof DecFloat34) {
      this.value = this.numberToScaled(value.getRaw());
    } else {
      this.set(value.get());
    }
    const magnitude = typeof this.value === "number" ? Math.abs(this.value) : this.value < 0n ? -this.value : this.value;
    const width = 2 * this.length - 1;
    if (typeof magnitude === "number" ? width <= 15 && magnitude >= NUMBER_POW10[width]
      : magnitude.toString().length > width) {
      throwError("CX_SY_ARITHMETIC_OVERFLOW");
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
    const value = this.getScaled();
    const negative = value < 0n;
    const mag = negative ? -value : value;
    const scaled = rescale(mag, this.decimals, decimals);
    const formatted = formatMag(scaled, decimals);
    return (negative ? "-" : "") + formatted;
  }

  public clear(): void {
    this.value = 0;
  }

  public get(): number {
    return Number(this.value) / Math.pow(10, this.decimals);
  }
}
