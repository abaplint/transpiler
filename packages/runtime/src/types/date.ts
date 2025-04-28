import {Hex} from "./hex";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {String} from "./string";
import {getDateFromNumber, getNumberFromDate} from "./_date_helper";
import {Float} from "./float";
import {parse} from "../operators/_parse";

export class Date implements ICharacter {
  private value: string;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.clear();
    this.qualifiedName = input?.qualifiedName;
  }

  public clone(): Date {
    const n = new Date({qualifiedName: this.qualifiedName});
    n.value = this.value;
    return n;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public set(value: INumeric | ICharacter | Hex | string | number) {
    if (typeof value === "number") {
      if (value <= 0 || value > 3652060) {
        this.value = "00000000";
      } else {
        this.value = getDateFromNumber(value);
      }
    } else if (value instanceof Float) {
      this.set(Math.round(value.getRaw()));
    } else if (typeof value === "string") {
      this.value = value;
    } else {
      this.set(value.get());
    }

    if (this.value.length > 8) {
      this.value = this.value.substr(0, 8);
    } else if (this.value.length < 8) {
      this.value = this.value.padEnd(8, " ");
    }

    return this;
  }

  public clear(): void {
    this.value = "00000000";
  }

  public get(): string {
    return this.value;
  }

  public getNumeric(): number {
    return getNumberFromDate(this.value);
  }

  public getOffset(input: {offset?: number | INumeric | Hex, length?: number | INumeric | Hex}) {
    if (input?.offset) {
      input.offset = parse(input.offset);
    }
    if (input?.length) {
      input.length = parse(input.length);
    }
    let ret = this.value;
    if (input?.offset) {
      // @ts-ignore
      ret = ret.substr(input.offset);
    }
    if (input?.length !== undefined) {
      // @ts-ignore
      ret = ret.substr(0, input.length);
    }
    const r = new String();
    r.set(ret);
    return r;
  }
}