import {parse} from "../operators/_parse";
import {throwError} from "../throw_error";
import {FieldSymbol} from "./field_symbol";
import {Hex} from "./hex";
import {Structure} from "./structure";
import {AbstractTypeData} from "./_abstract_type_data";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

// eslint-disable-next-line prefer-const
let featureFixLength = true;

export class Character implements ICharacter {
  private value: string;
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

  public set(value: ICharacter | string | Structure | FieldSymbol) {
    if (typeof value === "string" || typeof value === "number") {
      this.value = value;
    } else if (value instanceof FieldSymbol) {
      if (value.getPointer() === undefined) {
        throw "GETWA_NOT_ASSIGNED";
      }
      this.set(value.getPointer());
      return this;
    } else if (value instanceof Structure) {
      this.set(value.getCharacter());
      return this;
    } else {
      this.value = value.get() + "";
    }

    if (this.value.length > this.length) {
      this.value = this.value.substr(0, this.length);
    } else if (featureFixLength && this.value.length < this.length) {
      this.value.padEnd(this.length, " ");
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
    if (featureFixLength) {
      this.value = " ".repeat(this.length);
    } else {
      this.value = "";
    }
  }

  public get(): string {
    return this.value;
  }

  public getTrimEnd(): string {
    return this.value.replace(/ *$/, "");
  }

  public getOffset(input: {offset?: number | INumeric | Hex, length?: number | INumeric | Hex}) {
    if (input?.offset) {
      input.offset = parse(input.offset);
    }
    if (input?.length) {
      input.length = parse(input.length);
    }
    if ((input.offset && input.offset >= this.length)
         || (input.offset && input.offset < 0)
         || (input.length && input.length < 0)) {
      throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
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
    const r = new Character(ret.length);
    r.set(ret);
    return r;
  }
}