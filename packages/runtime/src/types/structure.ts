export class Structure {
  private readonly value: object;

  public constructor(fields: object) {
    this.value = fields;
  }

  /*
  public clear(): void {
    this.value = [];
  }
*/

/*
  public set(input: object) {

    return this;
  }
*/

  public get() {
    return this.value;
  }
}