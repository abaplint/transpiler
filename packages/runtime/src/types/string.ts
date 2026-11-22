import {parse} from "../operators/_parse";
import {throwError} from "../throw_error";
import {Character} from "./character";
import {FieldSymbol} from "./field_symbol";
import {Hex} from "./hex";
import {Integer} from "./integer";
import {Integer8} from "./integer8";
import {Packed} from "./packed";
import {Numc} from "./numc";
import {Structure} from "./structure";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {Float} from "./float";

export class String implements ICharacter {
  private value: string;
  private readonly qualifiedName: string | undefined;
  private readonly enumValues: readonly string[] | undefined;

  public constructor(input?: {qualifiedName?: string, enumValues?: readonly string[]}) {
    this.value = "";
    this.qualifiedName = input?.qualifiedName;
    this.enumValues = input?.enumValues;
  }

  public clone(): String {
    const n = new String({qualifiedName: this.qualifiedName, enumValues: this.enumValues});
    n.value = this.value;
    return n;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  /** the name of the constant this value stands for, when the type is an enumerated type
   *  declared in a class or interface: such a value is held as the position of its constant,
   *  and the initial value is the first constant */
  public getEnumName(): string | undefined {
    return this.enumValues?.[Number(this.value)];
  }

  public set(value: ICharacter | string | number | FieldSymbol): String {
    if (value instanceof FieldSymbol) {
      if (value.getPointer() === undefined) {
        throw new Error("GETWA_NOT_ASSIGNED");
      }
      return this.set(value.getPointer());
    } else if (typeof value === "string") {
      this.value = value;
    } else if (typeof value === "number") {
      this.value = value.toString();
    } else if (value instanceof Character) {
      // replace trailing blanks if the source is a Character string
      this.value = value.getTrimEnd();
    } else if (value instanceof Numc) {
      // after a character statement n can end with blanks, they are dropped as for c
      this.value = value.get().trimEnd();
    } else if (value instanceof Structure) {
      this.value = value.getCharacter().trimEnd();
    } else if (value instanceof Packed) {
      const fixed = (value as Packed).toFixed(value.getDecimals());
      const lv_sign = fixed.startsWith("-") ? "-" : " ";
      this.value = (fixed.startsWith("-") ? fixed.slice(1) : fixed) + lv_sign;
    } else if (value instanceof Integer) {
      const lv_sign = (value as Integer).get() >= 0 ? " " : "-";
      this.value = Math.abs((value as Integer).get()) + "";
      this.value += lv_sign;
    } else if (value instanceof Integer8) {
      // as for i: sign last, a blank for zero and positive; bigint keeps the int8 minimum exact
      const int8 = (value as Integer8).get();
      this.value = (int8 < 0n ? -int8 : int8) + ( int8 < 0n ? "-" : " " );
    } else if (value instanceof Float) {
      this.value = value.get() + "";
      this.value = this.value.replace(",", ".");
    } else {
      this.value = value.get() + "";
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

    if ((offset && offset > this.value.length)
        || (length && length > this.value.length)
        || (offset && length && offset + length > this.value.length)
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
    const r = new String();
    r.set(ret);
    return r;
  }
}
