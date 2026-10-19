import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {IOutputFile, ITranspilerOptions} from "../types";
import {TypeFactoryRegistry} from "../type_factory_registry";

// view, much like the tables
export class HandleView {
  public constructor(private readonly options?: ITranspilerOptions) {}

  public runObject(obj: abaplint.Objects.View, reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const type = obj.parseType(reg);
    const typeFactory = this.options?.sharedTypeFactories === true ? new TypeFactoryRegistry(filename) : undefined;

    const body = `abap.DDIC["${obj.getName().toUpperCase()}"] = {
  "objectType": "VIEW",
  "type": ${TranspileTypes.toTypeFunction(type, typeFactory)},
};`;
    const chunk = new Chunk(body);
    const helpers = typeFactory?.finalize(chunk) ?? "";
    const outputChunk = helpers === "" ? chunk : new Chunk(helpers).appendChunk(chunk);
// todo, "keyFields": ${JSON.stringify(obj.listKeys())},

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
