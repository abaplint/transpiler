export class I {
  private value: number;

  public constructor(value?: number) {
    this.value = value ? value : 0;
  }

  public set(value: I | number) {
    if (typeof value === "number") {
      this.value = value;
    } else {
      this.value = value.value;
    }
  }

  public add(value: I) {
    return new I(value.value + this.value);
  }

  public equals(value: I): boolean {
    return value.value === this.value;
  }

  public get(): number {
    return this.value;
  }
}