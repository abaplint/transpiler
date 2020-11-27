import {IFile, Transpiler} from "../packages/transpiler/src/";
import {ABAP} from "../packages/runtime/src/";

export async function runFiles(abap: ABAP, files: IFile[]) {
  const res = await new Transpiler().run(files);
  abap.console.clear();
  if (res.databaseSetup !== "") {
    await abap.initDB(res.databaseSetup);
  }
  return "global.abap = abap;\n" + res.objects[0].js.contents;
}