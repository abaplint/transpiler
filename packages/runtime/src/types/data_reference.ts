import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {ABAPObject} from "./abap_object";
import {Table} from "./table";
import {String} from "./string";
import {Structure} from "./structure";

type PointerType = INumeric | Table | ICharacter | ABAPObject | undefined | Structure;

export class DataReference  {
  private pointer: PointerType;
  // todo, add typing, so its possible to get runtime errors?

  public constructor() {
    this.pointer = undefined;
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
    return this.pointer?.clear();
  }

  public get() {
    // @ts-ignore
    return this.pointer?.get();
  }

  public array() {
    // @ts-ignore
    return this.pointer?.array();
  }

  public set(value: any) {
    return this.pointer?.set(value);
  }

  public getOffset(input: {offset: number, length: number}) {
    // Assuming we're interested in Strings here, for now...
    let ret = this.get();
    if (input?.offset) {
      ret = ret.substr(input.offset);
    }
    if (input?.length) {
      ret = ret.substr(0, input.length);
    }
    const r = new String();
    r.set(ret);
    return r;
  }
}