import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";
import {HandleTRAN} from "./handlers/handle_tran";

export {HandleTRAN};

class Extras implements ITranspilerPlugin {
  private readonly handlers: ITranspilerPlugin[] = [
    new HandleTRAN(),
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
