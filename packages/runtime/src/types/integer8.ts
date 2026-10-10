import {throwError} from "../throw_error";
import {Date} from "./date";
import {Float} from "./float";
import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {Integer, roundHalfAwayFromZero} from "./integer";
import {HexUInt8} from "./hex_uint8";
import {DecFloat34} from "./decfloat34";
import {Time} from "./time";
import {FieldSymbol} from "./field_symbol";

const digits = new RegExp(/^\s*-?\+?\d+\.?\d* *$/i);

export class Integer8 {
  private value: bigint;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.value = 0n;
    this.qualifiedName = input?.qualifiedName;
  }

  public clone(): Integer8 {
    const n = new Integer8({qualifiedName: this.qualifiedName});
    // set without trigger checks and padding
    n.value = this.value;
    return n;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public set(value: INumeric | ICharacter | Hex | string | number | bigint | Integer8 | Integer | Float | DecFloat34) {
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
        // a system clears the target before it raises, so the handler sees it initial
        this.value = 0n;
        throwError("CX_SY_CONVERSION_NO_NUMBER");
      }
      this.value = BigInt(value);
    } else if (value instanceof Date) {
// d is converted to the number of days since 01.01.0001, not the YYYYMMDD digits
      this.value = BigInt(value.getNumeric());
    } else if (value instanceof Float || value instanceof DecFloat34) {
      this.set(roundHalfAwayFromZero(value.getRaw()));
    } else if (value instanceof FieldSymbol) {
// dispatch on the type the field symbol points to, get() would only give its plain value
      if (value.getPointer() === undefined) {
        throw new Error("GETWA_NOT_ASSIGNED");
      }
      this.set(value.getPointer());
    } else if (value instanceof Time) {
// t is converted to the number of seconds since midnight, not the HHMMSS digits
      this.value = BigInt(value.getNumeric());
    } else if (value instanceof Hex || value instanceof XString || value instanceof HexUInt8) {
      const hex = value.get().slice(-16);
      this.value = BigInt.asIntN(64, hex === "" ? 0n : BigInt("0x" + hex));
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
