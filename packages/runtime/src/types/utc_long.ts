import {Hex} from "./hex";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class UTCLong implements ICharacter {
  private value: string;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.clear();
    this.qualifiedName = input?.qualifiedName;
  }

  public clone(): UTCLong {
    const n = new UTCLong({qualifiedName: this.qualifiedName});
    n.value = this.value;
    return n;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public getOffset(_input: { offset?: number | undefined; length?: number | undefined; }): ICharacter {
    throw new Error("Method not implemented, getOffset(), utcLong");
  }

  public set(_value: INumeric | ICharacter | Hex | string | number) {
// todo
    return this;
  }

  public clear(): void {
    this.value = "";
  }

  public get(): string {
    return this.value;
  }

}