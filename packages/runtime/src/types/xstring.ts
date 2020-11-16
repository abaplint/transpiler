import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class XString implements ICharacter {
  private value: string;

  public constructor() {
    this.value = "";
  }

  public set(value: ICharacter | INumeric | string) {
    if (typeof value === "string") {
      this.value = value;
    } else {
      const v = value.get();
      if (typeof v === "number") {
        this.value = v.toString(16);
      } else {
        this.value = v;
      }
    }
  }

  public clear(): void {
    this.value = "";
  }

  public get(input?: {offset: number, length: number}): string {
    let ret = this.value;
    if (input?.offset) {
      ret = ret.substr(input.offset * 2);
    }
    if (input?.length) {
      ret = ret.substr(0, input.length * 2);
    }
    return ret;
  }
}