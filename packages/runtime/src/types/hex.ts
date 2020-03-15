import {ICharacter} from "./_character";

export class Hex implements ICharacter {
  private value: string;
  private readonly length: number;

  public constructor(input?: {length?: number}) {
    this.value = "";
    this.length = input?.length ? input?.length : 1;
  }

  public set(_value: ICharacter | string) {
    console.dir("hex set, todo" + this.value + this.length);
  }

  public clear(): void {
    this.value = "";
  }

  public get(_input?: {offset: number, length: number}): string {
    return "hexTodo";
  }
}