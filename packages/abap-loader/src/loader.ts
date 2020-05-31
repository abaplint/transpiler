import * as transpiler from "@abaplint/transpiler";
import * as path from "path";

async function transpile(source: string) {
  const files: transpiler.IFile[] = [{filename: this.resourcePath, contents: source}];

  const dir = path.dirname(this.resourcePath);
  this.addContextDependency(dir);
  for (const f of this.fs.readdirSync(dir)) {
    const filename = dir + path.sep + f;
    if (filename === this.resourcePath) {
      continue;
    }
    files.push({filename, contents: this.fs.readFileSync(filename).toString()});
  }

  const t = new transpiler.Transpiler();
  return t.run(files);
}

export default async function ABAPLoader(source: string, _map: any, _meta: any) {
  const result: transpiler.IOutput[] = await transpile.bind(this)(source);

  const req = result[0].requires.map(e => "const " + e.name.toLowerCase() + " = require('./" + e.name.toLowerCase() + "." + e.type.toLowerCase() + ".abap')." + e.name.toLowerCase() + ";\n").join("");

  const content = req + result[0].js.contents;
  return content;
}