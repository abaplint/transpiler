export class I {
  private value: number;

  public constructor(value?: number) {
    this.value = value ? value : 0;
  }

  public set(value: I) {
    this.value = value.value;
  }

  public add(value: I) {
    return new I(value.value + this.value);
  }

  public get(): number {
    return this.value;
  }
}