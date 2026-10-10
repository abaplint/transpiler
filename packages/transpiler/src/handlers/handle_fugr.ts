import * as abaplint from "@abaplint/core";
import {IOutputFile, ITranspilerOptions} from "../types";
import {Traversal} from "../traversal";
import {Rearranger} from "../rearranger";
import {Chunk} from "../chunk";
import {TypeFactoryRegistry} from "../type_factory_registry";

export class HandleFUGR {
  private readonly options: ITranspilerOptions | undefined;

  public constructor(options?: ITranspilerOptions) {
    this.options = options;
  }

  public static shouldSkip(obj: abaplint.ABAPObject, reg: abaplint.IRegistry) {
    // @ts-ignore  todo
    return new abaplint.SkipLogic(reg).isGeneratedFunctionGroup(obj);
  }

  // function groups are compiled into a single file, with one closure for the function groups top variables
  public runObject(obj: abaplint.ABAPObject, reg: abaplint.IRegistry): IOutputFile[] {
    const spaghetti = new abaplint.SyntaxLogic(reg, obj).run().spaghetti;
    const chunk = new Chunk().appendString("{\n");
    const typeFactory = this.options?.sharedTypeFactories === true
      ? new TypeFactoryRegistry(obj.getName().toLowerCase().replace(/\//g, "#") + ".fugr.mjs") : undefined;


    if (HandleFUGR.shouldSkip(obj, reg)) {
      return [];
    }

    for (const file of obj.getSequencedFiles()) {
      typeFactory?.reserveSource(file.getRaw());
    }

    for (const file of obj.getSequencedFiles()) {
      if (this.options?.addFilenames === true) {
        chunk.appendString("// " + file.getFilename() + "\n");
      }

      const rearranged = new Rearranger().run(obj.getType(), file.getStructure());

      const contents = new Traversal(spaghetti, file, obj, reg, this.options, typeFactory).traverse(rearranged);
      chunk.appendChunk(contents);
      chunk.stripLastNewline();
    }
    const helpers = typeFactory?.finalize(chunk) ?? "";
    const assembled = helpers === "" ? chunk : new Chunk(helpers).appendChunk(chunk);
    // indentation must run once over the accumulated chunk: running it per
    // appended file re-indents the earlier files, drifting the brace counter
    // and shifting their mapping columns multiple times
    assembled.runIndentationLogic(this.options?.ignoreSourceMap);
    assembled.appendString("\n}");

    const output: IOutputFile = {
      object: {
        name: obj.getName(),
        type: obj.getType(),
      },
      filename: obj.getName().toLowerCase().replace(/\//g, "#") + ".fugr.mjs",
      chunk: assembled,
      requires: [],
      exports: [],
    };

    return [output];
  }

}
