import {ITranspilerOptions, Transpiler} from "../src";
import {UniqueIdentifier} from "../src/unique_identifier";

export async function runSingle(abap: string, options?: ITranspilerOptions): Promise<string | undefined> {
  UniqueIdentifier.reset();
  const res = await new Transpiler(options).run([{filename: "zfoobar.prog.abap", contents: abap}]);
  return res.objects[0]?.js.contents;
}

export async function runSingleMapped(abap: string, options?: ITranspilerOptions) {
  UniqueIdentifier.reset();
  const res = await new Transpiler(options).run([{filename: "zfoobar.prog.abap", contents: abap}]);
  const obj = res.objects[0];
  if (obj === undefined) {
    return undefined;
  }
  return {
    js: obj.js.contents,
    map: obj.sourceMap.contents,
  };
}