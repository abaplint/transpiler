import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {IOutputFile} from "../types";
import {TypeFactoryRegistry} from "../type_factory_registry";

export class HandleDataElement {
  public runObject(obj: abaplint.Objects.DataElement, reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const type = obj.parseType(reg);
    const typeFactory = new TypeFactoryRegistry(filename);

    let fixedValues: any | undefined = undefined;
    let doma: abaplint.Objects.Domain | undefined = undefined;
    if (obj.getDomainName()) {
      doma = reg.getObject("DOMA", obj.getDomainName()) as abaplint.Objects.Domain | undefined;
      if (doma) {
        fixedValues = doma.getFixedValues();
      }
    }

    const length = this.ddicNumber(obj, doma, "LENG");
    const decimals = this.ddicNumber(obj, doma, "DECIMALS");

    const body = `abap.DDIC["${obj.getName().toUpperCase()}"] = {
  "objectType": "DTEL",
  "type": ${TranspileTypes.toTypeFunction(type, typeFactory)},
  "domain": ${JSON.stringify(obj.getDomainName())},
  "datatype": ${JSON.stringify(obj.getDataType(reg))},
  "length": ${JSON.stringify(length)},
  "decimals": ${JSON.stringify(decimals)},
  "fixedValues": ${JSON.stringify(fixedValues)},
  "description": ${JSON.stringify(obj.getDescription())},
};`;
    const chunk = new Chunk(body);
    const helpers = typeFactory.finalize(chunk);
    const outputChunk = helpers === "" ? chunk : new Chunk(helpers).appendChunk(chunk);

    const output: IOutputFile = {
      object: {
        name: obj.getName(),
        type: obj.getType(),
      },
      filename: filename,
      chunk: outputChunk,
      requires: [],
      exports: [],
    };

    return [output];
  }

  /** The DDIC length or decimals, the runtime type cannot give them, eg. DEC 4 and DEC 5 are both 3 bytes packed.
   * Taken from the data element, or from its domain if the data element does not state it */
  private ddicNumber(obj: abaplint.Objects.DataElement, doma: abaplint.Objects.Domain | undefined,
                     tag: "LENG" | "DECIMALS"): number | undefined {
    const regex = new RegExp(`<${tag}>(\\d+)</${tag}>`);
    for (const raw of [obj.getXMLFile()?.getRaw(), doma?.getXMLFile()?.getRaw()]) {
      const match = raw?.match(regex);
      if (match) {
        return parseInt(match[1], 10);
      }
    }
    return undefined;
  }
}
