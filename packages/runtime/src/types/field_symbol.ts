import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {ABAPObject} from "./abap_object";
import {Table} from "./table";
import {String} from "./string";
import {Structure} from "./structure";

type PointerType = INumeric | Table | ICharacter | ABAPObject | undefined | Structure;

export class FieldSymbol  {
  private pointer: PointerType;
  private casting: boolean;
  // todo, add typing, so its possible to get runtime errors?

  public constructor() {
    this.pointer = undefined;
    this.casting = false;
  }

  public assign(pointer: PointerType) {
    this.pointer = pointer;
  }

  public setCasting() {
    this.casting =  true;
  }

  public unassign(): void {
    this.pointer = undefined;
  }

  public isAssigned(): boolean {
    return this.pointer !== undefined;
  }

  public getPointer() {
    return this.pointer;
  }

///////////////

  public clear() {
    return this.pointer?.clear();
  }

  public get() {
    if (this.casting) {
      // @ts-ignore
      return new String().set(Buffer.from(this.pointer?.get(), "hex").toString()).get();
    } else {
      // @ts-ignore
      return this.pointer?.get();
    }
  }

  public appendInitial() {
    if (this.pointer instanceof Table) {
      return this.pointer.appendInitial();
    }
    return undefined;
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
    if (input?.length !== undefined) {
      ret = ret.substr(0, input.length);
    }
    const r = new String();
    r.set(ret);
    return r;
  }
}