import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {Integer8} from "./integer8";

/*
function getNumberParts(x: number) {
  if(isNaN(x)) {
    throw "Float NaN";
  }
  const sig = x > 0 ? 1 : -1;
  if (!isFinite(x)) {
    throw "Float not finite";
  }
  x = Math.abs(x);
  const exp = Math.floor(Math.log(x) * Math.LOG2E) - 52;
  const man = x / Math.pow(2, exp);
  return {mantissa: sig * man, exponent: exp};
}
*/

export class Float {
  private value: number;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.value = 0;
    this.qualifiedName = input?.qualifiedName;
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
      this.value = parseFloat(value);
    } else if (value instanceof Integer8) {
      this.value = Number(value.get());
    } else if (value instanceof Float) {
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