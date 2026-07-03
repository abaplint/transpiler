import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";

export class HandleDDLS implements ITranspilerPlugin {

  public objectTypes(): string[] {
    return ["DDLS"];
  }

  public handleObject(obj: abaplint.IObject, _reg: abaplint.IRegistry, _options: ITranspilerOptions): IOutputFile[] | undefined {
    if (obj.getType() !== "DDLS") {
      return undefined;
    }

    // no runtime relevance, accept the object but produce no output
    return [];
  }

}
