import {throwError} from "../throw_error";
import {Float} from "./float";
import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {Integer} from "./integer";

const digits = new RegExp(/^\s*-?\+?\d+\.?\d* *$/i);

export class Integer8 {
  private value: bigint;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.value = 0n;
    this.qualifiedName = input?.qualifiedName;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public set(value: INumeric | ICharacter | Hex | string | number | bigint | Integer8 | Integer | Float) {
    if (typeof value === "number") {
      this.value = BigInt(value);
    } else if (typeof value === "bigint") {
      this.value = value;
    } else if (typeof value === "string") {
      if (value.endsWith("-")) {
        value = "-" + value.substring(0, value.length - 1);
      }
      if (value.trim().length === 0) {
        value = "0";
      } else if (digits.test(value) === false) {
        throwError("CX_SY_CONVERSION_NO_NUMBER");
      }
      this.value = BigInt(value);
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
    return this;
  }

  public clear(): void {
    this.value = 0n;
  }

  public get(): bigint {
    return this.value;
  }
}