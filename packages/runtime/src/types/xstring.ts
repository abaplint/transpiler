import {parse} from "../operators/_parse";
import {Float} from "./float";
import {Hex} from "./hex";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {Character} from "./character";
import {throwError} from "../throw_error";

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
      } else if (value instanceof Character) {
        this.set(value.getTrimEnd());
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
    let offset = input?.offset;
    if (offset) {
      offset = parse(offset);
    }

    let length = input?.length;
    if (length) {
      length = parse(length);
    }

    if ((offset && offset * 2 > this.value.length)
        || (length && length * 2 > this.value.length)
        || (offset && length && offset * 2 + length * 2 > this.value.length)
        || (offset && offset < 0)
        || (length && length < 0)) {
      throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
    }

    let ret = this.value;
    if (offset) {
      // @ts-ignore
      ret = ret.substr(offset * 2);
    }
    if (length !== undefined) {
      // @ts-ignore
      ret = ret.substr(0, length * 2);
    }
    const r = new XString();
    r.set(ret);
    return r;
  }
}