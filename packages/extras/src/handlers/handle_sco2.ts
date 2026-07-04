import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";

export class HandleSCO2 implements ITranspilerPlugin {

  public objectTypes(): string[] {
    return ["SCO2"];
  }

  public handleObject(obj: abaplint.IObject, _reg: abaplint.IRegistry, _options: ITranspilerOptions): IOutputFile[] | undefined {
    if (obj.getType() !== "SCO2") {
      return undefined;
    }

    // no runtime relevance, accept the object but produce no output
    return [];
  }

}
