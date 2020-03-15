import {ICharacter} from "./_character";

export class XString implements ICharacter {
  private value: string;

  public constructor() {
    this.value = "";
  }

  public set(_value: ICharacter | string) {
    console.dir("xstring set, todo" + this.value);
  }

  public clear(): void {
    this.value = "";
  }

  public get(_input?: {offset: number, length: number}): string {
    return "xstringGetTodo";
  }
}