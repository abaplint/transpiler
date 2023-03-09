import {parse} from "../operators/_parse";
import {throwError} from "../throw_error";
import {Character} from "./character";
import {FieldSymbol} from "./field_symbol";
import {Hex} from "./hex";
import {Integer} from "./integer";
import {Structure} from "./structure";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class String implements ICharacter {
  private value: string;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.value = "";
    this.qualifiedName = input?.qualifiedName;
  }

  public getQualifiedName() {
    return this.qualifiedName;
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
    } else if (value instanceof Structure) {
      this.value = value.getCharacter();
    } else if (value instanceof Integer) {
      const lv_sign = (parseInt(value.get(), 10) >= 0) ? " " : "-";
      this.value = value.get() + lv_sign;
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
    if (input?.offset) {
      input.offset = parse(input.offset);
    }
    if (input?.length) {
      input.length = parse(input.length);
    }
    if ((input.offset && input.offset > this.value.length)
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
    const r = new String();
    r.set(ret);
    return r;
  }
}