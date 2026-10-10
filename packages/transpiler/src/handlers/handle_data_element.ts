import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {IOutputFile, ITranspilerOptions} from "../types";
import {TypeFactoryRegistry} from "../type_factory_registry";

export class HandleDataElement {
  public constructor(private readonly options?: ITranspilerOptions) {}

  public runObject(obj: abaplint.Objects.DataElement, reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const type = obj.parseType(reg);
    const typeFactory = this.options?.sharedTypeFactories === true ? new TypeFactoryRegistry(filename) : undefined;

    let fixedValues: any | undefined = undefined;
    if (obj.getDomainName()) {
      const doma = reg.getObject("DOMA", obj.getDomainName()) as abaplint.Objects.Domain | undefined;
      if (doma) {
        fixedValues = doma.getFixedValues();
      }
    }

    const body = `abap.DDIC["${obj.getName().toUpperCase()}"] = {
  "objectType": "DTEL",
  "type": ${TranspileTypes.toTypeFunction(type, typeFactory)},
  "domain": ${JSON.stringify(obj.getDomainName())},
  "fixedValues": ${JSON.stringify(fixedValues)},
  "description": ${JSON.stringify(obj.getDescription())},
};`;
    const chunk = new Chunk(body);
    const helpers = typeFactory?.finalize(chunk) ?? "";
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
}
