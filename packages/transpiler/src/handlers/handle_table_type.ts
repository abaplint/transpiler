import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {TranspileTypes} from "../transpile_types.js";
import {IOutputFile} from "../types.js";

export class HandleTableType {
  public runObject(obj: abaplint.Objects.TableType, reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const type = obj.parseType(reg);

    const chunk = new Chunk().appendString(`abap.DDIC["${obj.getName().toUpperCase()}"] = {
  "objectType": "TTYP",
  "type": ${new TranspileTypes().toType(type)},
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