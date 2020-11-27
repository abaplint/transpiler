export class Console {
  private data = "";

  public clear(): void {
    this.data = "";
  }

  public add(data: string): void {
    this.data = this.data + data;
  }

  public get(): string {
    return this.data;
  }
}