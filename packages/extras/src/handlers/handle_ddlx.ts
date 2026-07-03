import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";

export class HandleDDLX implements ITranspilerPlugin {

  public objectTypes(): string[] {
    return ["DDLX"];
  }

  public handleObject(obj: abaplint.IObject, _reg: abaplint.IRegistry, _options: ITranspilerOptions): IOutputFile[] | undefined {
    if (obj.getType() !== "DDLX") {
      return undefined;
    }

    // metadata extensions have no runtime relevance, allow the object but produce no output
    return [];
  }

}
