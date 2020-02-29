export class Table  {
  // eslint-disable-next-line @typescript-eslint/prefer-readonly
  private value: any[];

  public constructor() {
    this.value = [];
  }

  public array(): any[] {
    return this.value;
  }

  public clear(): void {
    this.value = [];
  }

  public append(item: any) {
    this.value.push(item);
  }
}