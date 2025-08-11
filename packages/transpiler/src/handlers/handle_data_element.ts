import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {IOutputFile} from "../types";

export class HandleDataElement {
  public runObject(obj: abaplint.Objects.DataElement, reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const type = obj.parseType(reg);

    let fixedValues: any | undefined = undefined;
    if (obj.getDomainName()) {
      const doma = reg.getObject("DOMA", obj.getDomainName()) as abaplint.Objects.Domain | undefined;
      if (doma) {
        fixedValues = doma.getFixedValues();
      }
    }

    const chunk = new Chunk().appendString(`abap.DDIC["${obj.getName().toUpperCase()}"] = {
  "objectType": "DTEL",
  "type": ${TranspileTypes.toType(type)},
  "domain": ${JSON.stringify(obj.getDomainName())},
  "fixedValues": ${JSON.stringify(fixedValues)},
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