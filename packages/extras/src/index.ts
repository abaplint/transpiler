import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";
import {HandleDDLX} from "./handlers/handle_ddlx";

export {HandleDDLX};

class Extras implements ITranspilerPlugin {
  private readonly handlers: ITranspilerPlugin[] = [
    new HandleDDLX(),
  ];

  public objectTypes(): string[] {
    const ret: string[] = [];
    for (const handler of this.handlers) {
      ret.push(...handler.objectTypes());
    }
    return ret;
  }

  public handleObject(obj: abaplint.IObject, reg: abaplint.IRegistry, options: ITranspilerOptions): IOutputFile[] | undefined {
    for (const handler of this.handlers) {
      const handled = handler.handleObject(obj, reg, options);
      if (handled !== undefined) {
        return handled;
      }
    }
    return undefined;
  }
}

export const plugin: ITranspilerPlugin = new Extras();
