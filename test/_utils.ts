import {IFile, Transpiler} from "../packages/transpiler/src/";
import * as abap from "../packages/runtime/src/";

export async function runFiles(files: IFile[]) {
  const res = await new Transpiler().run(files);
  abap.Console.clear();
  return "global.abap = abap;\n" + res.objects[0].js.contents;
}