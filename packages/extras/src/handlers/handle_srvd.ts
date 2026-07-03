import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";

export class HandleSRVD implements ITranspilerPlugin {

  public objectTypes(): string[] {
    return ["SRVD"];
  }

  public handleObject(obj: abaplint.IObject, _reg: abaplint.IRegistry, _options: ITranspilerOptions): IOutputFile[] | undefined {
    if (obj.getType() !== "SRVD") {
      return undefined;
    }

    // no runtime relevance, accept the object but produce no output
    return [];
  }

}
