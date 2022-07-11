import {Float} from "./float";
import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class Integer implements INumeric {
  private value: number;

  public constructor() {
    this.value = 0;
  }

  public set(value: INumeric | ICharacter | Hex | string | number | Integer | Float) {
    if (typeof value === "number") {
      this.value = value;
    } else if (typeof value === "string" && value.trim().length === 0) {
      this.value = 0;
    } else if (typeof value === "string") {
      if (/^\s*-?\+?\d+\.?\d*$/i.test(value) === false) {
        // @ts-ignore
        if (abap.Classes["CX_SY_CONVERSION_NO_NUMBER"] !== undefined) {
          // @ts-ignore
          throw new abap.Classes["CX_SY_CONVERSION_NO_NUMBER"]();
        } else {
          throw "Global class CX_SY_CONVERSION_NO_NUMBER not found";
        }
      }
      this.value = parseInt(value, 10);
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
    this.value = 0;
  }

  public get(): number {
    return this.value;
  }
}