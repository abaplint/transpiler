import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {ABAPObject} from "./abap_object";

type PointerType = INumeric | ICharacter | ABAPObject | undefined;

export class FieldSymbol  {
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

///////////////

  public clear() {
    return this.pointer?.clear();
  }

  public get() {
    return this.pointer?.get();
  }

  public set(value: any) {
    return this.pointer?.set(value);
  }

}