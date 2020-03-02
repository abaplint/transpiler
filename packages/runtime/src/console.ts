export class Console {
  private static data = "";

  public static clear(): void {
    this.data = "";
  }

  public static add(data: string): void {
    this.data = this.data + data;
  }

  public static get(): string {
    return this.data;
  }
}