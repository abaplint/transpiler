export class UniqueIdentifier {
  public static counter = 0;

  public static reset() {
    this.counter = 0;
  }

  public static get(): string {
// todo: right now this is a gamble
    this.counter++;
    return "unique" + this.counter;
  }

}