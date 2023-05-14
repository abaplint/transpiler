import {Console} from "./console";

export class StandardOutConsole implements Console {
  private empty = true;

  public clear(): void {
    throw new Error("transpiler runtime: not supported for stdio console");
  }

  public add(data: string): void {
    process.stdout.write(data);
    this.empty = false;
  }

  public get(): string {
    return "";
  }

  public isEmpty() {
    return this.empty;
  }

  public getTrimmed(): string {
    throw new Error("transpiler runtime: not supported for stdio console");
  }
}