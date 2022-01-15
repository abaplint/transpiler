import * as abaplint from "@abaplint/core";
import {Chunk} from "./chunk";
import {IOutputFile} from "./types";

// tables or structures
export class HandleTable {
  public runObject(obj: abaplint.Objects.Table, _reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const chunk = new Chunk().appendString(`abap.DDIC['${obj.getName().toUpperCase()}'] = {
  "type": undefined
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