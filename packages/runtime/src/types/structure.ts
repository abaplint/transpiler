import {clone} from "../clone.js";
import {FieldSymbol} from "./field_symbol.js";
import {Table} from "./table.js";
import {ICharacter} from "./_character.js";
import {INumeric} from "./_numeric.js";
import {parse} from "../operators/_parse.js";
import {Hex} from "./hex.js";
import {Character} from "./character.js";
import {throwError} from "../throw_error.js";

export class Structure {
  private readonly value: any;
  private readonly qualifiedName: string | undefined;
  private readonly ddicName: string | undefined;

  public constructor(fields: any, qualifiedName?: string, ddicName?: string) {
    this.value = fields;
    this.qualifiedName = qualifiedName?.toUpperCase();
    this.ddicName = ddicName?.toUpperCase();
  }

  public clear() {
    for (const f in this.value) {
      // @ts-ignore
      this.value[f].clear();
    }
    return this;
  }

  public getDDICName() {
    return this.ddicName;
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
      throw "Structure, input is a table";
    } else if (input instanceof Structure) {
      const obj = input.get();
      const keys1 = Object.keys(obj);
      const keys2 = Object.keys(this.value);
      /*
      console.dir(keys1);
      console.dir(keys2);
*/
      for (let i = 0; i < keys1.length; i++) {
        const key1 = keys1[i];
        const key2 = keys2[i];
        this.value[key2].set(clone(obj[key1]));
      }
/*
      for (const f in obj) {
        // @ts-ignore
        this.value[f].set(clone(obj[f]));
      }
      */
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
      val += this.value[v].get();
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