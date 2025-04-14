import {Hex} from "./hex";
import {parse} from "../operators/_parse";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {throwError} from "../throw_error";
import {Float} from "./float";

export class Numc implements ICharacter {
  private value: string;
  private readonly length: number;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {length?: number, qualifiedName?: string}) {
    this.length = input?.length ? input?.length : 1;
    this.qualifiedName = input?.qualifiedName;
    this.clear();
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public set(value: INumeric | ICharacter | Hex | string | number, raw = false) {
    if (typeof value === "number") {
      this.value = Math.trunc(value) + "";
    } else if (typeof value === "string") {
      value = value.trim().replace(/[a-zA-Z]/g, "");
      if (value === "") {
        this.clear();
      } else {
        this.value = parseInt(value, 10) + "";
      }
    } else if (value instanceof Float) {
      this.value = Math.trunc(value.getRaw()) + "";
    } else {
      this.set(value.get());
      return this;
    }

    if (this.value.length > this.length) {
      this.value = this.value.substr(this.value.length - this.length, this.length);
    } else {
      const pad = this.length - this.value.length;
      if (pad > 0 && raw === false) {
        this.value = "0".repeat(pad) + this.value;
      }
    }

    return this;
  }

  public getLength() {
    return this.length;
  }

  public clear(): void {
    this.value = "0".repeat(this.length);
  }

  public get(): string {
    return this.value;
  }

  public getOffset(input: {offset?: number | INumeric | Hex, length?: number | INumeric | Hex}): Numc {
    let offset = input?.offset;
    if (offset) {
      offset = parse(offset);
    }

    let length = input?.length;
    if (length) {
      length = parse(length);
    }

    if ((offset && offset >= this.length)
         || (offset && offset < 0)
         || (length && length < 0)) {
      throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
    }

    let ret = this.value;
    if (offset) {
      ret = ret.substr(offset);
    }
    if (length !== undefined) {
      ret = ret.substr(0, length);
    }
    const r = new Numc({length: ret.length});
    r.set(ret);
    return r;
  }
}