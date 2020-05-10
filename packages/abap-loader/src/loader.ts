import * as transpiler from "@abaplint/transpiler";

export default async function ABAPLoader(source: any, _map: any, _meta: any) {
  const t = new transpiler.Transpiler();
  const result = await t.run([{filename: this.resourcePath, contents: source}]);
  return result.js[0].contents;
}