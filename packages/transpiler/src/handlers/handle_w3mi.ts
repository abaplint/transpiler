import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {IOutputFile} from "../types";
import {OutputLayout} from "../output_layout";
import {w3miObjectName} from "../w3mi_name";

export class HandleW3MI {
  public constructor(private readonly layout = new OutputLayout()) {
    // Asset paths are relative to the output root.
  }

  public runObject(obj: abaplint.Objects.WebMIME, _reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    obj.parse();
    const dataFile = obj.getDataFile();
    const dataFilename = dataFile ? this.layout.file({name: obj.getName(), type: obj.getType()}, dataFile.getFilename()) : undefined;
    const chunk = new Chunk().appendString(`abap.W3MI["${w3miObjectName(obj)}"] = {
  "objectType": "W3MI",
  "filename": ${JSON.stringify(dataFilename)},
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