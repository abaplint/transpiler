import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {IOutputFile} from "../types";

export class HandleOA2P {
  public runObject(obj: abaplint.Objects.Oauth2Profile, _reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const chunk = new Chunk().appendString(`abap.OA2P["${obj.getName().toUpperCase()}"] = {\n`);
    chunk.appendString(`  "scopes": ${JSON.stringify(obj.listScopes())},\n`);
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