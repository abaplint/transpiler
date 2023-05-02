import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {TranspileTypes} from "../transpile_types.js";
import {IOutputFile} from "../types.js";

// view, much like the tables
export class HandleView {
  public runObject(obj: abaplint.Objects.View, reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const type = obj.parseType(reg);

    const chunk = new Chunk().appendString(`abap.DDIC["${obj.getName().toUpperCase()}"] = {
  "objectType": "VIEW",
  "type": ${new TranspileTypes().toType(type)},
};`);
// todo, "keyFields": ${JSON.stringify(obj.listKeys())},

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