import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {IOutputFile} from "../types";

// tables or structures
export class HandleTable {
  public runObject(obj: abaplint.Objects.Table, reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const type = obj.parseType(reg);

    const chunk = new Chunk().appendString(`abap.DDIC["${obj.getName().toUpperCase()}"] = {
  "objectType": "TABL",
  "type": ${TranspileTypes.toType(type)},
  "keyFields": ${JSON.stringify(obj.listKeys(reg))},
  "description": ${JSON.stringify(obj.getDescription())},
};`);

    const output: IOutputFile = {
      object: {
        name: obj.getName(),
        type: obj.getType(),
      },
      filename: filename,
      chunk: chunk,
      requires: [],
      exports: [],
    };

    return [output];
  }
}