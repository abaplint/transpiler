import {ITranspilerOptions, Transpiler} from "../src";

export async function runSingle(abap: string, options?: ITranspilerOptions) {
  return new Transpiler(options).run(abap);
}