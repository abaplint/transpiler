export class UniqueIdentifier {
  public static counter = 0;

  public static reset() {
    this.counter = 0;
  }

  public static get(): string {
// as part of the validation, it is ensured that no identifiers exists
// in the input source with ^unique\d+$
    this.counter++;
    return "unique" + this.counter;
  }

}