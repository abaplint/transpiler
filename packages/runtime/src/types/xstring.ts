import {parse} from "../operators/_parse.js";
import {Float} from "./float.js";
import {Hex} from "./hex.js";
import {ICharacter} from "./_character.js";
import {INumeric} from "./_numeric.js";

export class XString implements ICharacter {
  private value: string;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.value = "";
    this.qualifiedName = input?.qualifiedName;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public set(value: ICharacter | INumeric | string | number) {
    if (typeof value === "string") {
      this.value = value;
      const finalLength = Math.ceil(this.value.length / 2 ) * 2;
      this.value = this.value.padEnd(finalLength, "0");
    } else if (typeof value === "number") {
      this.value = Math.round(value).toString(16);
      if (this.value.length % 2 === 1) {
        this.value = "0" + this.value;
      }
    } else {
      let v = value.get();
      if (value instanceof Float) {
        v = value.getRaw();
        this.set(v);
      } else if (typeof v === "number") {
        this.value = v.toString(16);
        const finalLength = Math.ceil(this.value.length / 2 ) * 2;
        this.value = this.value.padStart(finalLength, "0");
      } else {
        this.set(v);
      }
    }
    return this;
  }

  public clear(): void {
    this.value = "";
  }

  public get(): string {
    return this.value;
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
      ret = ret.substr(input.offset * 2);
    }
    if (input?.length !== undefined) {
      // @ts-ignore
      ret = ret.substr(0, input.length * 2);
    }
    const r = new XString();
    r.set(ret);
    return r;
  }
}