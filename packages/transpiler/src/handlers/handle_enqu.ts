import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {IOutputFile} from "../types";

export class HandleEnqu {
  public runObject(obj: abaplint.Objects.LockObject, _reg: abaplint.IRegistry): IOutputFile[] {

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const tableName = obj.getPrimaryTable();

    const chunk = new Chunk().appendString(`// enqueue object
abap.FunctionModules["ENQUEUE_${obj.getName().toUpperCase()}"] = async (INPUT) => {
  const lookup = abap.Classes["KERNEL_LOCK"];
  if (lookup === undefined) {
    throw new Error("Lock, kernel class missing");
  }
  const table_name = new abap.types.String({qualifiedName: "STRING"}).set("${tableName}" );
  const enqueue_name = new abap.types.String({qualifiedName: "STRING"}).set("${obj.getName().toUpperCase()}");
  await lookup.enqueue({table_name: table_name, enqueue_name: enqueue_name, input: INPUT.exporting});
};

abap.FunctionModules["DEQUEUE_${obj.getName().toUpperCase()}"] = async (INPUT) => {
  const lookup = abap.Classes["KERNEL_LOCK"];
  if (lookup === undefined) {
    throw new Error("Lock, kernel class missing");
  }
  const table_name = new abap.types.String({qualifiedName: "STRING"}).set("${tableName}" );
  const enqueue_name = new abap.types.String({qualifiedName: "STRING"}).set("${obj.getName().toUpperCase()}");
  await lookup.dequeue({table_name: table_name, enqueue_name: enqueue_name, input: INPUT.exporting});
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