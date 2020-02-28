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

  public get(): string {
    return this.value;
  }
}