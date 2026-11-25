import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {IOutputFile} from "../types";
import {TypeFactoryRegistry} from "../type_factory_registry";
import {CDSMetadata, ICDSMetadata} from "../cds_metadata";

// CDS entities: the row type like tables and views, plus the CDS metadata, eg. for XCO
export class HandleDataDefinition {
  public constructor(private readonly metadata: CDSMetadata) {}

  public runObject(obj: abaplint.Objects.DataDefinition, reg: abaplint.IRegistry): IOutputFile[] {
    if (obj.findSourceFile() === undefined) {
      return [];
    }
    const filename = obj.getName().toLowerCase().replace(/\//g, "#") + ".ddls.mjs";

    const cds = this.metadata.build(obj);
    const type = this.withoutAssociations(obj.parseType(reg), cds);
    const typeFactory = new TypeFactoryRegistry(filename);

    const body = `abap.DDIC["${obj.getName().toUpperCase()}"] = {
  "objectType": "DDLS",
  "type": ${TranspileTypes.toTypeFunction(type, typeFactory)},
  "keyFields": ${JSON.stringify(cds?.fields.filter(f => f.key).map(f => f.name.toUpperCase()) ?? [])},
  "description": ${JSON.stringify(obj.getDescription())},
  "sqlViewName": ${JSON.stringify(obj.getSQLViewName())},
  "cds": ${JSON.stringify(cds)},
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

  /** exposed associations are elements of the signature, but not components of the row type */
  private withoutAssociations(type: abaplint.AbstractType, cds: ICDSMetadata | undefined): abaplint.AbstractType {
    if (!(type instanceof abaplint.BasicTypes.StructureType) || cds === undefined) {
      return type;
    }
    const associations = new Set(cds.associations.map(a => a.name.toUpperCase()));
    const components = type.getComponents().filter(c => !associations.has(c.name.toUpperCase()));
    if (components.length === type.getComponents().length) {
      return type;
    }
    return new abaplint.BasicTypes.StructureType(components, type.getQualifiedName(), type.getDDICName(), type.getDescription());
  }
}
