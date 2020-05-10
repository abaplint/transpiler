import * as transpiler from "@abaplint/transpiler";

export default async function(source: any, _map: any, _meta: any) {
  const t = new transpiler.Transpiler();
  console.dir(t);
  console.dir(this.resourcePath);
  console.dir(source);
  console.log("resource path: " + this.resourcePath);
  const result = await t.run([{filename: this.resourcePath, contents: source}]);
  console.dir(result);

  /*
  return "const abap = require('@abaplint/runtime');\n" +
    "export " + result.js[0].contents;
    */
}