import {clone} from "../clone";
import {FieldSymbol} from "./field_symbol";
import {Table} from "./table";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {parse} from "../operators/_parse";
import {Hex} from "./hex";
import {Character} from "./character";

export class Structure {
  private readonly value: any;
  private readonly qualifiedName: string | undefined;

  public constructor(fields: any, qualifiedName?: string) {
    this.value = fields;
    this.qualifiedName = qualifiedName?.toUpperCase();
  }

  public clear() {
    for (const f in this.value) {
      // @ts-ignore
      this.value[f].clear();
    }
    return this;
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
    if (input?.offset) {
      input.offset = parse(input.offset);
    }
    if (input?.length) {
      input.length = parse(input.length);
    }

    const val = this.getCharacter();

    if ((input.offset && input.offset >= val.length)
         || (input.offset && input.offset < 0)
         || (input.length && input.length < 0)) {
      // @ts-ignore
      if (abap.Classes["CX_SY_RANGE_OUT_OF_BOUNDS"] !== undefined) {
        // @ts-ignore
        throw new abap.Classes["CX_SY_RANGE_OUT_OF_BOUNDS"]();
      } else {
        throw "Global class CX_SY_RANGE_OUT_OF_BOUNDS not found";
      }
    }

    let ret = val;
    if (input?.offset) {
      // @ts-ignore
      ret = ret.substr(input.offset);
    }
    if (input?.length !== undefined) {
      // @ts-ignore
      ret = ret.substr(0, input.length);
    }
    const r = new Character({length: ret.length});
    r.set(ret);
    return r;
  }
}