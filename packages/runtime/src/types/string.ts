import {ICharacter} from "./_character";

export class String implements ICharacter {
  private value: string;

  public constructor() {
    this.value = "";
  }

  public set(value: ICharacter | string) {
    if (typeof value === "string") {
      this.value = value;
    } else {
      this.value = value.get();
    }
    return this;
  }

  public clear(): void {
    this.value = "";
  }

  public get(input?: {offset: number, length: number}): string {
    let ret = this.value;
    if (input?.offset) {
      ret = ret.substr(input.offset);
    }
    if (input?.length) {
      ret = ret.substr(0, input.length);
    }
    return ret;
  }
}