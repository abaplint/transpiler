import * as abaplint from "@abaplint/core";
import {DatabaseSetupResult, IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";
import {CDSDatabaseView} from "./cds_database_view";

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

  public amendDatabaseSetup(dbSetup: DatabaseSetupResult, reg: abaplint.IRegistry, _options: ITranspilerOptions): void {
    for (const obj of reg.getObjects()) {
      // The CLI bundles core, whereas this plugin loads core from node_modules.
      // Object identity must not depend on those being the same module instance.
      if (obj.getType() !== "DDLS") {
        continue;
      }

      const view = new CDSDatabaseView(reg).build(obj as abaplint.Objects.DataDefinition);
      if (view !== undefined) {
        dbSetup.schemas.sqlite.push(view);
        dbSetup.schemas.pg.push(view);
        dbSetup.schemas.snowflake.push(view);
      }
    }
  }

}
