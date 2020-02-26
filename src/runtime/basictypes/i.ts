export class i {
  private value: number;

  public constructor(value?: number) {
    this.value = value ? value : 0;
  }

  public set(value: i) {
    this.value = value.value;
  }

  public add(value: i) {
    return new i(value.value + this.value);
  }

  public get(): number {
    return this.value;
  }
}