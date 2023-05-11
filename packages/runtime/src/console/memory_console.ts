import {Console} from "./console";

export class MemoryConsole implements Console {
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

  public getTrimmed(): string {
    return this.data.split("\n").map(a => a.trimEnd()).join("\n");
  }
}