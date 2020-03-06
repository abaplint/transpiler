import {ICharacter} from "./_character";

export class Character implements ICharacter {
  private value: string;
  private readonly length: number;

  public constructor(input?: {value?: string, length?: number}) {
    this.value = input?.value ? input?.value : "";
    this.length = input?.length ? input?.length : 1;
  }

  public set(value: ICharacter | string) {
    if (typeof value === "string" || typeof value === "number") {
      this.value = value;
    } else {
      this.value = value.get();
    }
    if (this.value.length > this.length) {
      this.value.substr(0, this.length);
    }
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