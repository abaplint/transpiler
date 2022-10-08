import {FieldSymbol} from "./field_symbol";

export class ABAPObject  {
  private value: any | undefined;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.qualifiedName = input?.qualifiedName;
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

  public set(value: ABAPObject | any) {
    if (value instanceof ABAPObject) {
      this.value = value.get();
    } else if (value instanceof FieldSymbol) {
      this.value = value.getPointer().get();
    } else {
      this.value = value;
    }
  }
}