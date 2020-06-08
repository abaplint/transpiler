export class Structure {
  private readonly value: any;

  public constructor(fields: any) {
    this.value = fields;
  }

  public clear() {
    for (const f in this.value) {
      // @ts-ignore
      this.value[f].clear();
    }
    return this;
  }

  public set(input: Structure) {
    const obj = input.get();
    for (const f in obj) {
      // @ts-ignore
      this.value[f].set(obj[f].get());
    }
    return this;
  }

  public get() {
    return this.value;
  }
}