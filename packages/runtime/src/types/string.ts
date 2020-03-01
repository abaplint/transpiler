import {ICharacter} from "./_character";

export class String implements ICharacter {
  private value: string;

  public constructor(input?: {value?: string}) {
    this.value = input?.value ? input?.value : "";
  }

  public set(value: ICharacter | string) {
    if (typeof value === "string") {
      this.value = value;
    } else {
      this.value = value.get();
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