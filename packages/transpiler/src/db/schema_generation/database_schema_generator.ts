import * as abaplint from "@abaplint/core";

export interface DatabaseSchemaGenerator {
  buildVIEW(view: abaplint.Objects.View): string;
  buildTABL(view: abaplint.Objects.Table): string;
}