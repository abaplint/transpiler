import {clone as cloneAny} from "../clone";
import {FieldSymbol} from "./field_symbol";
import {Table} from "./table";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {parse} from "../operators/_parse";
import {Hex} from "./hex";
import {Character} from "./character";
import {throwError} from "../throw_error";

export class Structure {
  private readonly value: {[key: string]: any};
  private readonly qualifiedName: string | undefined;
  private readonly ddicName: string | undefined;
  private readonly suffix: { [key: string]: string };
  private readonly asInclude: { [key: string]: boolean };

  public constructor(fields: {[key: string]: any}, qualifiedName?: string, ddicName?: string, suffix?: any, asInclude?: any) {
    this.value = fields;
    this.qualifiedName = qualifiedName?.toUpperCase();
    this.ddicName = ddicName?.toUpperCase();
    this.suffix = suffix;
    this.asInclude = asInclude;
  }

  public clone(): Structure {
    const newValues: {[key: string]: any} = {};
    for (const key in this.value) {
      newValues[key] = cloneAny(this.value[key]);
    }

    const n = new Structure(newValues, this.qualifiedName, this.ddicName, this.suffix, this.asInclude);
    return n;
  }

  public clear() {
    for (const f in this.value) {
      this.value[f].clear();
    }
    return this;
  }

  public getDDICName() {
    return this.ddicName;
  }

  public getRenamingSuffix() {
    return this.suffix;
  }

  public getAsInclude() {
    return this.asInclude;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public set(input: Structure | string | INumeric | Table | ICharacter | FieldSymbol | undefined) {
    if (input === undefined) {
      return;
    }

    if (input instanceof FieldSymbol) {
      this.set(input.getPointer());
    } else if (input instanceof Table) {
      throw new Error("Structure, input is a table");
    } else if (input instanceof Structure) {
      const obj = input.get();
      const keys1 = Object.keys(obj);
      const keys2 = Object.keys(this.value);
      for (let i = 0; i < keys1.length; i++) {
        const key1 = keys1[i];
        const key2 = keys2[i];
        this.value[key2].set(cloneAny(obj[key1]));
      }
    } else {
      this.setCharacter(input);
    }

    return this;
  }

  private setCharacter(input: string | ICharacter | INumeric) {
    this.clear();

    let val = input;
    if (typeof val !== "string") {
      val = val.get() + "";
    }

    for (const key of Object.keys(this.value)) {
      const targetLength = this.value[key].getLength();
      this.value[key].set(val.substr(0, targetLength));
      val = val.substr(targetLength);
    }
  }

  public get() {
    return this.value;
  }

  public getCharacter(): string {
    let val = "";
    for (const v in this.value) {
      if (this.value[v] instanceof Structure) {
        val += this.value[v].getCharacter();
      } else {
        val += this.value[v].get();
      }
    }
    return val;
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

    const val = this.getCharacter();

    if ((offset && offset >= val.length)
         || (offset && offset < 0)
         || (length && length < 0)) {
      throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
    }

    let ret = val;
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