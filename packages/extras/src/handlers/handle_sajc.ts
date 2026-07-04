import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";

export class HandleSAJC implements ITranspilerPlugin {

  public objectTypes(): string[] {
    return ["SAJC"];
  }

  public handleObject(obj: abaplint.IObject, _reg: abaplint.IRegistry, _options: ITranspilerOptions): IOutputFile[] | undefined {
    if (obj.getType() !== "SAJC") {
      return undefined;
    }

    // no runtime relevance, accept the object but produce no output
    return [];
  }

}
