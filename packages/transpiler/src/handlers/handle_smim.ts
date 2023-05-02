import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {IOutputFile} from "../types.js";

export class HandleSMIM {
  public runObject(obj: abaplint.Objects.MIMEObject, _reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    obj.parse();
    const dataFile = obj.getDataFile();
    const chunk = new Chunk().appendString(`abap.SMIM["${obj.getName().toUpperCase()}"] = {
  "objectType": "SMIM",
  "filename": ${JSON.stringify(dataFile?.getFilename())},
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

    const ret = [output];

    if (dataFile) {
      const data: IOutputFile = {
        object: {
          name: obj.getName(),
          type: obj.getType(),
        },
        filename: dataFile?.getFilename(),
        chunk: new Chunk().appendString(dataFile?.getRaw()),
        requires: [],
        exports: [],
      };
      ret.push(data);
    }

    return ret;
  }
}