import {Hex} from "./hex";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {String} from "./string";
import {Float} from ".";
import {parse} from "../operators/_parse";

export class Time implements ICharacter {
  private value: string;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.clear();
    this.qualifiedName = input?.qualifiedName;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public set(value: INumeric | ICharacter | Hex | string | number) {
    if (typeof value === "number") {
      const date = new Date();
      date.setTime(value * 1000);
      this.value = date.getUTCHours().toString().padStart(2,"0") +
                   date.getUTCMinutes().toString().padStart(2,"0") +
                   date.getUTCSeconds().toString().padStart(2,"0");
    } else if (typeof value === "string") {
      this.value = value;
      if (this.value.length > 6) {
        this.value = this.value.substring(0, 6);
      }
    } else if (value instanceof String) {
      this.set(value.get().padEnd(6, "0"));
    } else if (value instanceof Float) {
      this.set(Math.round(value.getRaw()));
    } else {
      this.set(value.get());
    }
    return this;
  }

  public clear(): void {
    this.value = "000000";
  }

  public get(): string {
    return this.value;
  }

  public getNumeric(): number {
    const hours = parseInt(this.value.substr(0,2),10);
    const minutes = parseInt(this.value.substr(2,2),10);
    const seconds = parseInt(this.value.substr(4,2),10);
    return hours * 3600 + minutes * 60 + seconds;
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