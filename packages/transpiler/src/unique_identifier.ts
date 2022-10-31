export class UniqueIdentifier {
  private static counter = 0;
  private static indexBackup = 0;

  public static reset() {
    this.counter = 0;
  }

  public static get(): string {
// as part of the validation, it is ensured that no identifiers exists
// in the input source with ^unique\d+$
    this.counter++;
    return "unique" + this.counter;
  }

  public static resetIndexBackup() {
    this.indexBackup = 0;
  }

  public static getIndexBackup(): string {
    this.indexBackup++;
    return "indexBackup" + this.indexBackup;
  }

  public static getIndexBackup1(): string {
    return "indexBackup1";
  }

}