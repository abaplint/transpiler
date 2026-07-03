import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";

export class HandleSIA6 implements ITranspilerPlugin {

  public objectTypes(): string[] {
    return ["SIA6"];
  }

  public handleObject(obj: abaplint.IObject, _reg: abaplint.IRegistry, _options: ITranspilerOptions): IOutputFile[] | undefined {
    if (obj.getType() !== "SIA6") {
      return undefined;
    }

    // no runtime relevance, accept the object but produce no output
    return [];
  }

}
