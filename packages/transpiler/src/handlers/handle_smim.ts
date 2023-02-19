import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {IOutputFile} from "../types";

export class HandleSMIM {
  public runObject(obj: abaplint.Objects.MIMEObject, _reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    obj.parse();
    const chunk = new Chunk().appendString(`abap.SMIM["${obj.getName().toUpperCase()}"] = {
  "objectType": "SMIM",
  "filename": ${JSON.stringify(obj.getDataFile()?.getFilename())},
  "url": ${JSON.stringify(obj.getURL())},
  "class": ${JSON.stringify(obj.getClass())},
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