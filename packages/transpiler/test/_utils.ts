import {ITranspilerOptions, Transpiler} from "../src";

export async function runSingle(abap: string, options?: ITranspilerOptions) {
  const res = await new Transpiler(options).run([{filename: "zfoobar.prog.abap", contents: abap}]);
  return res.js[0]?.contents;
}