import * as fs from "fs";
import * as path from "path";
import * as glob from "glob";
import * as childProcess from "child_process";
import * as os from "os";
import * as Transpiler from "@abaplint/transpiler";
import {ITranspilerConfig, TranspilerConfig} from "./config";
import {FileOperations} from "./file_operations";

function loadFiles(config: ITranspilerConfig): Transpiler.IFile[] {
  const files: Transpiler.IFile[] = [];
  const filter = (config.input_filter ?? []).map(pattern => new RegExp(pattern, "i"));
  let skipped = 0;
  for (let filename of glob.sync(config.input_folder + "/**", {nosort: true, nodir: true})) {
    if (filter.length > 0 && filter.some(a => a.test(filename)) === false) {
      skipped++;
      continue;
    }
    const contents = fs.readFileSync(filename, "utf8");
    filename = path.basename(filename);
    files.push({filename, contents});
    console.log("Add:\t" + filename);
  }
  console.log(skipped + " files skipeed");
  return files;
}

function loadLib(config: ITranspilerConfig): Transpiler.IFile[] {
  const files: Transpiler.IFile[] = [];
  if (config.lib && config.lib !== "") {
    console.log("Clone: " + config.lib + "\n");
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "abap_transpile-"));
    childProcess.execSync("git clone --quiet --depth 1 " + config.lib + " .", {cwd: dir, stdio: "inherit"});
    for (let filename of glob.sync(dir + "/src/**", {nosort: true, nodir: true})) {
      const contents = fs.readFileSync(filename, "utf8");
      filename = path.basename(filename);
      files.push({filename, contents});
      console.log("Add lib: " + filename);
    }
    FileOperations.deleteFolderRecursive(dir);
  }
  return files;
}

async function run() {
  const config = TranspilerConfig.find();

  console.log("Transpiler CLI");

  const files = loadFiles(config).concat(loadLib(config));

  console.log("\nBuilding");
  const t = new Transpiler.Transpiler(config.options);
  const output = await t.run(files);

  console.log("\nOutput");
  const outputFolder = config.output_folder;
  if (!fs.existsSync(outputFolder)) {
    fs.mkdirSync(outputFolder);
  }

  for (const o of output.objects) {
    console.log(o.js.filename);
    fs.writeFileSync(outputFolder + path.sep + o.js.filename, o.js.contents);
  }

  if (config.write_unit_tests === true) {
    fs.writeFileSync(outputFolder + path.sep + "index.js", output.unitTest);
  }
}

run().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});