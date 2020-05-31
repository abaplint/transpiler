import {ITranspilerOptions, Transpiler} from "../src";

export async function runSingle(abap: string, options?: ITranspilerOptions): Promise<string | undefined> {
  const res = await new Transpiler(options).run([{filename: "zfoobar.prog.abap", contents: abap}]);
  return res[0]?.js.contents;
}