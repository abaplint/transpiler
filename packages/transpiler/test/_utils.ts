import {Transpiler} from "../src/index.js";
import {IFile, ITranspilerOptions} from "../src/types.js";
import {UniqueIdentifier} from "../src/unique_identifier.js";
import * as abaplint from "@abaplint/core";

async function runFiles(files: IFile[], options?: ITranspilerOptions) {
  const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
  const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
  const res = await new Transpiler(options).run(reg);
  return res;
}

export async function runSingle(abap: string, options?: ITranspilerOptions): Promise<string | undefined> {
  UniqueIdentifier.reset();
  const res = await runFiles([{filename: "zfoobar.prog.abap", contents: abap}], options);
  return res.objects[0]?.chunk.getCode();
}

export async function runSingleMapped(abap: string, options?: ITranspilerOptions, filename = "zfoobar.prog.abap") {
  UniqueIdentifier.reset();
  const res = await runFiles([{filename, contents: abap}], options);
  const obj = res.objects[0];
  if (obj === undefined) {
    return undefined;
  }
  return {
    js: obj.chunk.getCode(),
    map: obj.chunk.getMap("zfoobar.prog.mjs"),
  };
}