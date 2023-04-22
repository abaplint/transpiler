import {FieldSymbol} from "./field_symbol";

export class ABAPObject  {
  private value: any | undefined;
  private readonly qualifiedName: string | undefined;
  private readonly internalName: string | undefined;

  public constructor(input?: {qualifiedName?: string, internalName?: string}) {
    this.qualifiedName = input?.qualifiedName;
    this.internalName = input?.internalName;
    this.clear();
  }

  public get() {
    return this.value;
  }

  public clear() {
    this.value = undefined;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public getInternalName() {
    return this.internalName;
  }

  public set(value: ABAPObject | any) {
    if (value instanceof ABAPObject) {
      this.value = value.get();
    } else if (value instanceof FieldSymbol) {
      this.value = value.getPointer().get();
    } else {
      this.value = value;
    }
    return this;
  }
}