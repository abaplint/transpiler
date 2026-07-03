// PolyForm Noncommercial License 1.0.0, see LICENSE.md
// Required Notice: Copyright Heliconia Labs ApS (hello@heliconialabs.com)
import * as abaplint from "@abaplint/core";
import {DatabaseSetupResult, IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";
import {HandleAPLO} from "./handlers/handle_aplo";
import {HandleBDEF} from "./handlers/handle_bdef";
import {HandleDDLS} from "./handlers/handle_ddls";
import {HandleDDLX} from "./handlers/handle_ddlx";
import {HandleSCO2} from "./handlers/handle_sco2";
import {HandleSIA6} from "./handlers/handle_sia6";
import {HandleSRVD} from "./handlers/handle_srvd";

export {HandleAPLO, HandleBDEF, HandleDDLS, HandleDDLX, HandleSCO2, HandleSIA6, HandleSRVD};

class Extras implements ITranspilerPlugin {
  private readonly handlers: ITranspilerPlugin[] = [
    new HandleAPLO(),
    new HandleBDEF(),
    new HandleDDLS(),
    new HandleDDLX(),
    new HandleSCO2(),
    new HandleSIA6(),
    new HandleSRVD(),
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

  public amendDatabaseSetup(dbSetup: DatabaseSetupResult, reg: abaplint.IRegistry, options: ITranspilerOptions): void {
    for (const handler of this.handlers) {
      handler.amendDatabaseSetup?.(dbSetup, reg, options);
    }
  }
}

// PolyForm Noncommercial License 1.0.0
// Required Notice: Copyright Heliconia Labs ApS (hello@heliconialabs.com)
export const plugin: ITranspilerPlugin = new Extras();
