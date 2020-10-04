export class ABAPObject  {
  private value: any | undefined;

  public constructor() {
    this.value = undefined;
  }

  public get() {
    return this.value;
  }

  public set(value: ABAPObject | any) {
    if (value instanceof ABAPObject) {
      this.value = value.get();
    } else {
      this.value = value;
    }
  }
}