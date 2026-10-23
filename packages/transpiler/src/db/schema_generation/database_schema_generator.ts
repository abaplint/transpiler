import * as abaplint from "@abaplint/core";

export interface DatabaseSchemaGenerator {
  buildVIEW(view: abaplint.Objects.View): string;
  buildTABL(view: abaplint.Objects.Table): string;
}

/** "IF NOT EXISTS" when the dialect accepts it on this statement and the caller asked for it */
export function ifNotExistsClause(ifNotExists: boolean): string {
  return ifNotExists ? "IF NOT EXISTS " : "";
}

export function packedTypeToDatabase(type: abaplint.BasicTypes.PackedType): string {
  const precision = type.getLength() * 2 - 1;
  return `DECIMAL(${precision},${type.getDecimals()})`;
}
