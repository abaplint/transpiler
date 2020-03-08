export class ABAPObject  {
  private value: any | undefined;

  public constructor() {
    this.value = undefined;
  }

  public get() {
    return this.value;
  }

  public set(value: any) {
    this.value = value;
  }
}