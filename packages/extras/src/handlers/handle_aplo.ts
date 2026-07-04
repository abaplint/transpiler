import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";

export class HandleAPLO implements ITranspilerPlugin {

  public objectTypes(): string[] {
    return ["APLO"];
  }

  public handleObject(obj: abaplint.IObject, _reg: abaplint.IRegistry, _options: ITranspilerOptions): IOutputFile[] | undefined {
    if (obj.getType() !== "APLO") {
      return undefined;
    }

    // no runtime relevance, accept the object but produce no output
    return [];
  }

}
