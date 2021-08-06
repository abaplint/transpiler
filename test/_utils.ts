import {IFile, Transpiler} from "../packages/transpiler/src/";
import {ABAP} from "../packages/runtime/src/";

// see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncFunction
// eslint-disable-next-line @typescript-eslint/no-empty-function
export const AsyncFunction = Object.getPrototypeOf(async ()=> {}).constructor;

export async function runFiles(abap: ABAP, files: IFile[]) {
  const res = await new Transpiler().run(files);
  abap.console.clear();
  if (res.databaseSetup !== "") {
    await abap.initDB(res.databaseSetup);
  }
  return "global.abap = abap;\n" + res.objects[0].chunk.getCode();
}
