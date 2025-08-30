import {FieldSymbol} from "./field_symbol";

export class ABAPObject  {
  private value: any | undefined;
  private readonly qualifiedName: string | undefined;
  private readonly RTTIName: string | undefined;

  public constructor(input?: {qualifiedName?: string, RTTIName?: string}) {
    this.qualifiedName = input?.qualifiedName;
    this.RTTIName = input?.RTTIName;
    this.clear();
  }

  public clone(): ABAPObject {
    const n = new ABAPObject({qualifiedName: this.qualifiedName, RTTIName: this.RTTIName});
    n.value = this.value;
    return n;
  }

  public get() {
    return this.value;
  }

  public getInternalID(): string | undefined {
    return "OpenABAPInternalObjectId=" + this.value?.INTERNAL_ID;
  }

  public clear() {
    this.value = undefined;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public getRTTIName() {
    return this.RTTIName;
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