import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions} from "../types";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {Traversal} from "../traversal";
import {ConstantsTranspiler} from "../structures";
import {TypeFactoryRegistry} from "../type_factory_registry";

export class HandleTypePool {

  public constructor(private readonly options?: ITranspilerOptions) {}

  public runObject(obj: abaplint.ABAPObject, reg: abaplint.IRegistry): IOutputFile[] {
    const spaghetti = new abaplint.SyntaxLogic(reg, obj).run().spaghetti;
    const spaghettiNode = spaghetti.getFirstChild()?.getFirstChild();
    if (spaghettiNode === undefined) {
      throw new Error("HandleTypePool: no spaghetti found");
    }

    const abapFile = obj.getABAPFiles()[0];
    if (abapFile === undefined) {
      throw new Error("HandleTypePool: no ABAP file found");
    }

    const chunk = new Chunk();
    chunk.appendString(`const pool = {};\n`);
    const typeFactory = this.options?.sharedTypeFactories === true
      ? new TypeFactoryRegistry(obj.getName().toLowerCase() + ".typepool.mjs") : undefined;
    typeFactory?.reserveSource(abapFile.getRaw());

    for (const v in spaghettiNode?.getData().vars) {
      const abs = spaghettiNode!.getData().vars[v];
      const name = `pool['${v.toLowerCase()}']`;
      chunk.appendString(`${name} = ${TranspileTypes.toType(abs.getType(), undefined, typeFactory)};\n`);
      chunk.appendString(Traversal.setValues(abs, name));

      // yea, this is a mess
      for (const cons of abapFile.getStructure()?.findAllStructures(abaplint.Structures.Constants) || []) {
        const cName = cons.findFirstExpression(abaplint.Expressions.DefinitionName)?.getFirstToken().getStr().toLowerCase();
        if (cName === v.toLocaleLowerCase()) {
          chunk.appendString(ConstantsTranspiler.handleValues(name, cons,
            new Traversal(spaghetti, abapFile, obj, reg, this.options, typeFactory)));
        }
      }
    }
    for (const t in spaghettiNode?.getData().types) {
      const abs = spaghettiNode!.getData().types[t];
      chunk.appendString(`pool['${t.toLowerCase()}'] = ${TranspileTypes.toType(abs.getType(), undefined, typeFactory)};\n`);
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

    if (typeFactory !== undefined && typeFactory.size > 0) {
      const helpers = typeFactory.finalize(output.chunk);
      if (helpers !== "") {
        output.chunk = new Chunk(helpers).appendChunk(output.chunk);
      }
    }

    return [output];
  }

}
