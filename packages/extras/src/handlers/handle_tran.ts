import * as abaplint from "@abaplint/core";
import {Chunk, IOutputFile, ITranspilerOptions, ITranspilerPlugin} from "@abaplint/transpiler";

export class HandleTRAN implements ITranspilerPlugin {

  public objectTypes(): string[] {
    return ["TRAN"];
  }

  public handleObject(obj: abaplint.IObject, _reg: abaplint.IRegistry, _options: ITranspilerOptions): IOutputFile[] | undefined {
    if (obj.getType() !== "TRAN") {
      return undefined;
    }

    const xmlFile = obj.getXMLFile();
    const filename = xmlFile?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (xmlFile === undefined || filename === undefined) {
      return [];
    }

    const raw = xmlFile.getRaw();
    const tcode = raw.match(/<TCODE>(.+)<\/TCODE>/)?.[1] || obj.getName().toUpperCase();
    const program = raw.match(/<PGMNA>(.+)<\/PGMNA>/)?.[1] || "";

    const chunk = new Chunk()
      .appendString(`abap.TRAN = abap.TRAN || {};\n`)
      .appendString(`abap.TRAN["${tcode}"] = {"program": "${program}"};`);

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
