import {ICharacter} from "./_character";

export class XString implements ICharacter {
  private value: string;

  public constructor() {
    this.value = "";
  }

  public set(value: ICharacter | string) {
    // todo, this wont work?
    if (typeof value === "string") {
      this.value = value;
    } else {
      this.value = value.get();
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