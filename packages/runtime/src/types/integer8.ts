import {throwError} from "../throw_error";
import {Float} from "./float";
import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {Integer} from "./integer";
import {getBit} from "../statements/get_bit";
import {Character} from "./character";

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
      if (value.get().length === 16) {
        const lv_bit = new Character();
        getBit(new Integer().set(1), value, lv_bit);
        if (lv_bit.get() === "1") {
          const val = BigInt("0x" + value.get());
          this.value = val - BigInt("0x10000000000000000");
        } else {
          this.value = BigInt("0x" + value.get());
        }
      } else {
// todo, what if the input is longer than 16 bytes?
        this.value = BigInt("0x" + value.get());
      }
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