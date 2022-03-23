import {ICharacter} from "./_character";

export class Character implements ICharacter {
  private value: string;
  private readonly length: number;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {length?: number, qualifiedName?: string}) {
    this.value = "";
    this.length = input?.length ? input?.length : 1;
    this.qualifiedName = input?.qualifiedName;
  }

  public set(value: ICharacter | string) {
    if (typeof value === "string" || typeof value === "number") {
      this.value = value;
    } else {
      this.value = value.get() + "";
    }
    if (this.value.length > this.length) {
      this.value = this.value.substr(0, this.length);
    }
    return this;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public getLength() {
    return this.length;
  }

  public clear(): void {
    this.value = "";
  }

  public get(): string {
    return this.value;
  }

  public getOffset(input: {offset?: number, length?: number}) {
    let ret = this.value;
    if (input?.offset) {
      ret = ret.substr(input.offset);
    }
    if (input?.length !== undefined) {
      ret = ret.substr(0, input.length);
    }
    const r = new Character({length: ret.length});
    r.set(ret);
    return r;
  }
}