import * as abaplint from "@abaplint/core";

export interface DatabaseSchemaGenerator {
  buildVIEW(view: abaplint.Objects.View): string;
  buildTABL(view: abaplint.Objects.Table): string;
}

export function packedTypeToDatabase(type: abaplint.BasicTypes.PackedType): string {
  const precision = type.getLength() * 2 - 1;
  return `DECIMAL(${precision},${type.getDecimals()})`;
}
