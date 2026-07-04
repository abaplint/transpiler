import * as abaplint from "@abaplint/core";
import {DatabaseSetupResult, IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";

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
      if (!(obj instanceof abaplint.Objects.DataDefinition)) {
        continue;
      }

      const view = this.buildView(obj, reg, "'");
      if (view !== undefined) {
        dbSetup.schemas.sqlite.push(view);
      }

      const quotedView = this.buildView(obj, reg, "\"");
      if (quotedView !== undefined) {
        dbSetup.schemas.pg.push(quotedView);
        dbSetup.schemas.snowflake.push(quotedView);
      }
    }
  }

  private buildView(obj: abaplint.Objects.DataDefinition, reg: abaplint.IRegistry, quote: string): string | undefined {
    if (obj.getParsedData() === undefined) {
      obj.parse();
    }
    const parsed = obj.getParsedData();
    const source = parsed?.sources[0];
    if (parsed === undefined || source === undefined || parsed.fields.length === 0) {
      return undefined;
    }

    const table = reg.getObject("TABL", source.name);
    if (!(table instanceof abaplint.Objects.Table)) {
      return undefined;
    }

    const q = (name: string) => quote + name.toLowerCase() + quote;
    const viewName = parsed.definitionName || obj.getName();
    const columns = parsed.fields.map(field =>
      q(source.name) + "." + q(field.name) + " AS " + field.name.toLowerCase()).join(", ");

    return `CREATE VIEW ${q(viewName)} AS SELECT ${columns} FROM ${q(source.name)};`;
  }

}
