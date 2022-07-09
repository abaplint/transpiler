import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {ABAPObject} from "./abap_object";
import {Table} from "./table";
import {String} from "./string";
import {Structure} from "./structure";
import {Float} from "./float";

type PointerType = INumeric | Table | ICharacter | ABAPObject | undefined | Structure | Float;

export class DataReference  {
  private pointer: PointerType;
  private readonly type: PointerType;

  public constructor(type: PointerType) {
    this.pointer = undefined;
    this.type = type;
  }

  public getType(): PointerType {
    return this.type;
  }

  public assign(pointer: PointerType) {
    this.pointer = pointer;
  }

  public unassign(): void {
    this.pointer = undefined;
  }

  public getPointer() {
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

  public set(value: any) {
    if (value instanceof DataReference) {
      return this.pointer = value.getPointer();
    } else {
      return this.pointer?.set(value);
    }
  }

  public getOffset(input: {offset: number, length: number}) {
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