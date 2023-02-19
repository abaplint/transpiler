import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {IOutputFile} from "../types";

export class HandleW3MI {
  public runObject(obj: abaplint.Objects.WebMIME, _reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    obj.parse();
    const dataFile = obj.getDataFile();
    const chunk = new Chunk().appendString(`abap.W3MI["${obj.getName().toUpperCase()}"] = {
  "objectType": "W3MI",
  "filename": ${JSON.stringify(dataFile?.getFilename())},
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