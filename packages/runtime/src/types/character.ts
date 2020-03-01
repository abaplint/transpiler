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

  public eq(value: ICharacter | string): boolean {
    if (typeof value === "string") {
      return this.value === value;
    } else {
      return this.value === value.get();
    }
  }

  public ne(value: ICharacter | string): boolean {
    return !this.eq(value);
  }

  public get(): string {
    return this.value;
  }
}