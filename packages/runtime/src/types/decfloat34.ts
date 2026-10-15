import {Float} from ".";
import {Date} from "./date";
import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {Time} from "./time";
import {FieldSymbol} from "./field_symbol";
import {Packed} from "./packed";
import {Integer8} from "./integer8";
import {throwError} from "../throw_error";
import {Decimal, decimalFromBigInt, decimalFromNumber, decimalToNumber, decimalToString, parseDecimal} from "./_decimal";

export class DecFloat34 {
  // exact, a double cannot hold 34 digits
  private value: Decimal;

  public constructor() {
    this.value = {digits: 0n, scale: 0};
  }

  public clone(): DecFloat34 {
    const n = new DecFloat34();
    n.value = this.value;
    return n;
  }

  public set(value: INumeric | ICharacter | Hex | string | number | bigint | DecFloat34) {
    if (typeof value === "number") {
      this.value = decimalFromNumber(value);
    } else if (typeof value === "bigint") {
      // what an int8 gives from get()
      this.value = decimalFromBigInt(value);
    } else if (typeof value === "string") {
      // get() writes the decimal separator as a comma, it is read back as well
      const parsed = parseDecimal(value);
      if (parsed === undefined) {
        // a system clears the target before it raises, so the handler sees it initial
        this.clear();
        throwError("CX_SY_CONVERSION_NO_NUMBER");
      }
      this.value = parsed!;
    } else if (value instanceof DecFloat34) {
      this.value = value.getDecimal();
    } else if (value instanceof Packed) {
      // exact, get() would go through a double
      this.value = parseDecimal(value.toFixed(value.getDecimals()))!;
    } else if (value instanceof Integer8) {
      this.value = decimalFromBigInt((value as Integer8).get() as unknown as bigint);
    } else if (value instanceof Date) {
// d is converted to the number of days since 01.01.0001, not the YYYYMMDD digits
      this.value = decimalFromNumber(value.getNumeric());
    } else if (value instanceof Float) {
      this.value = decimalFromNumber(value.getRaw());
    } else if (value instanceof FieldSymbol) {
// dispatch on the type the field symbol points to, get() would only give its plain value
      if (value.getPointer() === undefined) {
        throw new Error("GETWA_NOT_ASSIGNED");
      }
      this.set(value.getPointer());
    } else if (value instanceof Time) {
// t is converted to the number of seconds since midnight, not the HHMMSS digits
      this.value = decimalFromNumber(value.getNumeric());
    } else if (value instanceof Hex || value instanceof XString) {
// todo, how/if should this work?
      const parsed = parseInt(value.get(), 16);
      this.set(isNaN(parsed) ? 0 : parsed);
    } else {
      this.set(value.get());
    }
    return this;
  }

  public setDecimal(value: Decimal): DecFloat34 {
    this.value = value;
    return this;
  }

  public clear(): void {
    this.value = {digits: 0n, scale: 0};
  }

  /** The exact value */
  public getDecimal(): Decimal {
    return this.value;
  }

  /** The nearest double */
  public getRaw(): number {
    return decimalToNumber(this.value);
  }

  /** The exact value as text with a period, as string templates write it */
  public toText(): string {
    return decimalToString(this.value);
  }

  public get(): string {
    return decimalToString(this.value, ",");
  }
}
