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

  const options: Transpiler.ITranspilerOptions = {
    ignoreSyntaxCheck: false,
    addFilenames: true,
    addCommonJS: true,
  };
  console.log("\nBuilding");
  const t = new Transpiler.Transpiler(options);
  const output = await t.run(files);

  console.log("\nOutput");
  const outputFolder = "output";
  if (!fs.existsSync(outputFolder)) {
    fs.mkdirSync(outputFolder);
  }

  for (const o of output.objects) {
    console.log(o.js.filename);
    fs.writeFileSync(outputFolder + path.sep + o.js.filename, o.js.contents);
  }
  fs.writeFileSync(outputFolder + path.sep + "index.js", output.unitTest);
}

run().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});