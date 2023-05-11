import {Console} from "./console";

export class StandardOutConsole implements Console {

  public clear(): void {
    throw new Error("transpiler runtime: not supported for stdio console");
  }

  public add(data: string): void {
    console.log(data);
  }

  public get(): string {
    throw new Error("transpiler runtime: not supported for stdio console");
  }

  public getTrimmed(): string {
    throw new Error("transpiler runtime: not supported for stdio console");
  }
}