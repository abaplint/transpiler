import * as Transpiler from "@abaplint/transpiler";
import * as fs from "fs";
import * as path from "path";
import * as glob from "glob";

async function run() {
  console.log("Transpiler CLI");

  const files: Transpiler.IFile[] = [];
  for (let filename of glob.sync("src/**", {nosort: true, nodir: true})) {
    const contents = fs.readFileSync(filename, "utf8");
    filename = path.basename(filename);
    files.push({filename, contents});
    console.log(filename);
  }

  const t = new Transpiler.Transpiler();
  const output = await t.run(files);

  const dir = "output";
  if (!fs.existsSync(dir)){
    fs.mkdirSync(dir);
  }
  for (const o of output) {
    console.log(o.js.filename);
    let contents = o.js.contents;
    for (const r of o.requires) {
      contents += "Requires: " + r.name + r.type + "\n";
    }
    for (const e of o.exports) {
      contents += "Export: " + e + "\n";
    }
    fs.writeFileSync(dir + path.sep + o.js.filename, contents);
  }
}

run().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});