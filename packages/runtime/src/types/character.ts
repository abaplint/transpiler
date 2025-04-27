import {parse} from "../operators/_parse";
import {throwError} from "../throw_error";
import {FieldSymbol} from "./field_symbol";
import {Hex} from "./hex";
import {Structure} from "./structure";
import {AbstractTypeData} from "./_abstract_type_data";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {Integer} from "./integer";

const TRIMREGEX = / *$/;

export class Character implements ICharacter {
  private value: string;
  private constant: boolean = false;
  private readonly length: number;
  private readonly extra: AbstractTypeData | undefined;

  public constructor(length?: number, extra?: AbstractTypeData) {
    this.length = length || 1;
    if (typeof this.length === "object") {
      throw "Character, invalid length, object: " + JSON.stringify(this.length);
    } else if (this.length <= 0) {
      throw "Character, invalid length, less than zero";
    }
    this.extra = extra;
    this.clear();
  }

  public clone(obj: Character): Character {
    const n = new Character(obj.getLength(), obj.extra);
    // set without trigger checks and padding
    n.value = obj.value;
    return n;
  }

  public setConstant() {
    this.constant = true;
    return this;
  }

  public set(value: ICharacter | string | Structure | FieldSymbol | Integer) {
    if (this.constant === true) {
      throw new Error("Changing constant");
    }

    if (typeof value === "string") {
      this.value = value;
    } else if (typeof value === "number") {
      this.value = value + "";
    } else if (value instanceof FieldSymbol) {
      if (value.getPointer() === undefined) {
        throw new Error("GETWA_NOT_ASSIGNED");
      }
      this.set(value.getPointer());
      return this;
    } else if (value instanceof Structure) {
      this.set(value.getCharacter());
      return this;
    } else if (value instanceof Integer) {
      this.value = Math.abs( value.get() ) + ( value.get() < 0 ? "-" : " " );
      this.value = this.value.padStart(this.length, " ");
    } else {
      this.value = value.get() + "";
    }

    if (this.value.length > this.length) {
      this.value = this.value.substr(0, this.length);
    } else if (this.value.length < this.length) {
      this.value = this.value.padEnd(this.length, " ");
    }
    return this;
  }

  public getQualifiedName() {
    return this.extra?.qualifiedName;
  }

  public getConversionExit() {
    return this.extra?.conversionExit;
  }

  public getDDICName() {
    return this.extra?.ddicName;
  }

  public getLength() {
    return this.length;
  }

  public clear(): void {
    this.value = " ".repeat(this.length);
  }

  public get(): string {
    return this.value;
  }

  public getTrimEnd(): string {
    if (this.value.endsWith(" ") === true) {
      return this.value.replace(TRIMREGEX, "");
    } else {
      return this.value;
    }
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

    if ((offset && offset >= this.length)
        || (length && length > this.length)
        || (offset && length && offset + length > this.length)
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
    const r = new Character(ret.length);
    r.set(ret);
    return r;
  }
}