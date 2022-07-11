import {Hex} from "./hex";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class UTCLong implements ICharacter {
  private value: string;

  public constructor() {
    this.clear();
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