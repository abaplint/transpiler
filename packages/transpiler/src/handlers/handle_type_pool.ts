import * as abaplint from "@abaplint/core";
import {IOutputFile} from "../types";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {Traversal} from "../traversal";

export class HandleTypePool {

  public runObject(obj: abaplint.ABAPObject, reg: abaplint.IRegistry): IOutputFile[] {
    const spaghetti = new abaplint.SyntaxLogic(reg, obj).run().spaghetti.getFirstChild()?.getFirstChild();

    const chunk = new Chunk();
    chunk.appendString(`const pool = {};\n`);

    for (const v in spaghetti?.getData().vars) {
      const abs = spaghetti!.getData().vars[v];
      const name = `pool['${v.toLowerCase()}']`;
      chunk.appendString(`${name} = ${new TranspileTypes().toType(abs.getType())};\n`);
      console.dir(abs);
      chunk.appendString(Traversal.setValues(abs, name));
    }
    for (const t in spaghetti?.getData().types) {
      const abs = spaghetti!.getData().types[t];
      chunk.appendString(`pool['${t.toLowerCase()}'] = ${new TranspileTypes().toType(abs.getType())};\n`);
    }

    chunk.appendString(`abap.TypePools['${obj.getName()}'] = pool;`);

    const output: IOutputFile = {
      object: {
        name: obj.getName(),
        type: obj.getType(),
      },
      filename: obj.getName().toLowerCase() + "." + obj.getType().toLowerCase() + ".mjs",
      chunk: chunk,
      requires: [],
      exports: [],
    };

    return [output];
  }

}