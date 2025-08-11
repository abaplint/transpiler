import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {ABAPObject} from "./abap_object";
import {Table} from "./table";
import {String} from "./string";
import {Structure} from "./structure";
import {Float} from "./float";
import {Hex} from "./hex";
import {parse} from "../operators/_parse";
import {FieldSymbol} from "./field_symbol";
import {Integer8} from "./integer8";

type PointerType = INumeric | Table | ICharacter | ABAPObject | Integer8 | undefined | Structure | Float;

export class DataReference  {
  private pointer: PointerType;
  private readonly type: PointerType;

  public constructor(type: PointerType) {
    this.pointer = undefined;
    this.type = type;
  }

  public clone(): DataReference {
    const n = new DataReference(this.type);
    n.pointer = this.pointer;
    return n;
  }

  public getType(): PointerType {
    return this.type;
  }

  public assign(pointer: PointerType) {
    this.pointer = pointer;
    return this;
  }

  public unassign(): void {
    this.pointer = undefined;
  }

  public getPointer() {
    return this.pointer;
  }

  public dereference() {
    return this.pointer;
  }

///////////////

  public clear() {
    this.unassign();
//    return this.pointer?.clear();
  }

  public get() {
    if (this.pointer === this) {
      throw "Cyclic data reference";
    }
    // @ts-ignore
    return this.pointer?.get();
  }

  public array() {
    // @ts-ignore
    return this.pointer?.array();
  }

  public getArrayLength() {
    // @ts-ignore
    return this.pointer?.getArrayLength();
  }

  public set(value: any) {
    if (value instanceof DataReference) {
      this.pointer = value.getPointer();
      return this;
    } else if (value instanceof FieldSymbol) {
      if (value.getPointer() === undefined) {
        throw new Error("GETWA_NOT_ASSIGNED");
      } else if (value.getPointer() instanceof DataReference) {
        this.pointer = value.getPointer().getPointer();
        return this;
      } else {
        throw new Error("OBJECTS_MOVE_NOT_SUPPORTED");
      }
    }
    return this.pointer?.set(value);
  }

  public getOffset(input: {offset?: number | INumeric | Hex, length?: number | INumeric | Hex}) {
    if (input?.offset) {
      input.offset = parse(input.offset);
    }
    if (input?.length) {
      input.length = parse(input.length);
    }
    // Assuming we're interested in Strings here, for now...
    let ret = this.get();
    if (input?.offset) {
      ret = ret.substr(input.offset);
    }
    if (input?.length !== undefined) {
      ret = ret.substr(0, input.length);
    }
    const r = new String();
    r.set(ret);
    return r;
  }
}