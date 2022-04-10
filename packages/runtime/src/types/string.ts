import {Character} from "./character";
import {Integer} from "./integer";
import {ICharacter} from "./_character";

export class String implements ICharacter {
  private value: string;

  public constructor() {
    this.value = "";
  }

  public set(value: ICharacter | string | number) {
    if (typeof value === "string") {
      this.value = value;
    } else if (typeof value === "number") {
      this.value = value.toString();
    } else if (value instanceof Character) {
      // replace trailing blanks if the source is a Character string
      this.value = value.get().replace(/[ ]*$/g, "");
    } else if (value instanceof Integer) {
      const lv_sign = (parseInt(value.get(), 10) >= 0) ? " " : "-";
      this.value = value.get() + lv_sign;
    }

    else {
      this.value = value.get() + "";
    }
    return this;
  }

  public clear(): void {
    this.value = "";
  }

  public get(): string {
    return this.value;
  }

  public getOffset(input: { offset: number, length: number }) {
    if (input.offset && input.offset > this.value.length) {
      // @ts-ignore
      if (abap.Classes["CX_SY_RANGE_OUT_OF_BOUNDS"] !== undefined) {
        // @ts-ignore
        throw new abap.Classes["CX_SY_RANGE_OUT_OF_BOUNDS"]();
      } else {
        throw "Global class CX_SY_RANGE_OUT_OF_BOUNDS not found";
      }
    }

    let ret = this.value;
    if (input?.offset) {
      ret = ret.substr(input.offset);
    }
    if (input?.length !== undefined) {
      ret = ret.substr(0, input.length);
    }
    const r = new String();
    r.set(ret);
    return r;
  }
}