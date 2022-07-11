import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {ABAPObject} from "./abap_object";
import {Table} from "./table";
import {String} from "./string";
import {Structure} from "./structure";
import {Hex} from "./hex";

type PointerType = INumeric | Table | ICharacter | ABAPObject | undefined | Structure;

export class FieldSymbol  {
  private pointer: PointerType;
  private casting: boolean;
  private readonly type: PointerType;

  public constructor(type?: PointerType) {
    this.pointer = undefined;
    this.casting = false;
    this.type = type;
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
    if (this.casting) {
      // todo, this wont work for everything, eg changing CASTING'ed values
      return this.get();
    }
    return this.pointer;
  }

///////////////

  public clear() {
    return this.pointer?.clear();
  }

  public get() {
    if (this.casting) {
      if (this.type instanceof Hex) {
        // @ts-ignore
        const ret = new String().set(Buffer.from(this.pointer?.get(), "utf16le").toString("hex"));
        return ret.get();
      } else {
        // @ts-ignore
        const ret = new String().set(Buffer.from(this.pointer?.get(), "hex").toString("utf16le"));
        return ret.get();
      }
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
    this.pointer?.set(value);
    return this;
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