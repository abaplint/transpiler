import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {ABAPObject} from "./abap_object";
import {Table} from "./table";
import {String} from "./string";
import {Structure} from "./structure";
import {Hex} from "./hex";
import {Float} from "./float";
import {DataReference} from "./data_reference";
import {HexUInt8} from "./hex_uint8";
import {Date} from "./date";
import {Character} from "./character";

type PointerType = INumeric | Table | ICharacter | ABAPObject | undefined | Structure | Float;

export class FieldSymbol  {
  private pointer: PointerType;
  private casting: boolean;
  private readonly type: PointerType;

  public constructor(type?: PointerType) {
    this.pointer = undefined;
    this.casting = false;
    this.type = type;
  }

  public clone() {
    throw new Error("FieldSymbol cannot be cloned");
  }

  public getQualifiedName() {
    // @ts-ignore
    return this.type.getQualifiedName();
  }

  public getRTTIName() {
    return this.type instanceof ABAPObject ? this.type.getRTTIName() : undefined;
  }

  public assign(pointer: PointerType) {
    this.pointer = pointer;
  }

  public getType() {
    return this.type;
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
    const casted = this.castToStructure();
    if (casted !== undefined) {
      return casted;
    }
    if (this.needsReinterpretation()) {
      // todo, this wont work for everything, eg changing CASTING'ed values
      return this.get();
    }
    return this.pointer;
  }

  public dereference() {
    if (this.pointer instanceof DataReference) {
      return this.pointer.getPointer();
    } else {
      return this.pointer;
    }
  }

///////////////

  public clear() {
    return this.pointer?.clear();
  }

  public get() {
    const casted = this.castToStructure();
    if (casted !== undefined) {
      return casted.get();
    }
    if (this.needsReinterpretation()) {
      if (this.type instanceof Hex || this.type instanceof HexUInt8) {
        const pt = this.pointer;
        if (pt instanceof Float) {
          const buf = Buffer.allocUnsafe(8);
// CASTING is platform specific, so perhaps add a setting? But anyhow its not something developers should use
          buf.writeDoubleLE(pt.getRaw());
          return buf.toString("hex").toUpperCase();
        } else {
          const ret = new String().set(Buffer.from(this.pointer?.get(), "utf16le").toString("hex"));
          return ret.get();
        }
      } else {
        const ret = new String().set(Buffer.from(this.pointer?.get(), "hex").toString("utf16le"));
        return ret.get();
      }
    } else {
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

  public getArrayLength() {
    // @ts-ignore
    return this.pointer?.getArrayLength();
  }

  public set(value: any) {
    const casted = this.castToStructure();
    if (casted !== undefined) {
      casted.set(value);
      (this.pointer as Character | Structure).set(casted.getCharacter());
      return this;
    }
    if (this.needsReinterpretation()) {
      if (this.type instanceof Hex || this.type instanceof HexUInt8) {
        const pt = this.pointer;
        if (pt instanceof Float) {
          const buf = Buffer.from(value.get(), "hex");
          pt.set(buf.readDoubleLE());
          return;
        }
      }
    }

    this.pointer?.set(value);
    return this;
  }

  public getOffset(input: {offset?: number | INumeric | Hex, length?: number | INumeric | Hex}) {
    return this.getPointer().getOffset(input);
  }

  /** a character-like field CASTING to a flat character-like structure: the structure laid over the
   *  field's characters, component by component. It is a copy, changes to single components are not
   *  written back to the field */
  private castToStructure(): Structure | undefined {
    if (this.casting === false || !(this.type instanceof Structure)) {
      return undefined;
    }
    const pt = this.pointer;
    let chars: string;
    if (pt instanceof Character) {
      chars = pt.get();
    } else if (pt instanceof Structure) {
      chars = pt.getCharacter();
    } else {
      return undefined;
    }
    return this.type.clone().set(chars);
  }

  private needsReinterpretation(): boolean {
    // Date-to-date CASTING shares the original storage and must retain the Date
    // wrapper so arithmetic uses day counts instead of parsing YYYYMMDD as a number.
    return this.casting && !(this.type instanceof Date && this.pointer instanceof Date);
  }
}
