import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {IOutputFile} from "../types";

export class HandleMSAG {
  public runObject(obj: abaplint.Objects.MessageClass, _reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const chunk = new Chunk().appendString(`abap.MSAG["${obj.getName().toUpperCase()}"] = {\n`);
    for (const m of obj.getMessages()) {
      chunk.appendString(`  "${m.getNumber()}": "${m.getMessage().replace(/"/g, `\\"`)}",\n`);
    }
    chunk.appendString(`};`);

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