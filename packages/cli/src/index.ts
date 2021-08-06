import * as fs from "fs";
import * as path from "path";
import * as glob from "glob";
import * as childProcess from "child_process";
import * as os from "os";
import * as ProgressBar from "progress";
import * as Transpiler from "@abaplint/transpiler";
import {ITranspilerConfig, TranspilerConfig} from "./config";
import {FileOperations} from "./file_operations";

class Progress implements Transpiler.IProgress {
  private bar: ProgressBar;

  public set(total: number, _text: string) {
    this.bar = new ProgressBar(":percent - :elapseds - :text", {total, renderThrottle: 100});
  }

  public async tick(text: string) {
    this.bar.tick({text});
    this.bar.render();
  }
}

function loadLib(config: ITranspilerConfig): Transpiler.IFile[] {
  const files: Transpiler.IFile[] = [];
  if (config.lib && config.lib !== "") {
    console.log("Clone: " + config.lib);
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "abap_transpile-"));
    childProcess.execSync("git clone --quiet --depth 1 " + config.lib + " .", {cwd: dir, stdio: "inherit"});
    let count = 0;
    for (let filename of glob.sync(dir + "/src/**", {nosort: true, nodir: true})) {
      const contents = fs.readFileSync(filename, "utf8");
      filename = path.basename(filename);
      files.push({filename, contents});
      count++;
    }
    console.log(count + " files added from lib");
    FileOperations.deleteFolderRecursive(dir);
  }
  return files;
}

function writeObjects(objects: Transpiler.IOutputFile[], writeSourceMaps: boolean, outputFolder: string) {
  for (const o of objects) {
    let contents = o.chunk.getCode();
    if (writeSourceMaps === true) {
      const name = o.filename + ".map";
      contents = contents + `\n//# sourceMappingURL=` + name;
      fs.writeFileSync(outputFolder + path.sep + name, o.chunk.getMap(o.filename));
    }
    fs.writeFileSync(outputFolder + path.sep + o.filename, contents);
  }
}

async function run() {
  console.log("Transpiler CLI");

  const config = TranspilerConfig.find(process.argv[2]);
  const files = FileOperations.loadFiles(config).concat(loadLib(config));

  console.log("\nBuilding");
  const t = new Transpiler.Transpiler(config.options);
  const output = await t.run(files, new Progress());

  console.log("\nOutput");
  const outputFolder = config.output_folder;
  if (!fs.existsSync(outputFolder)) {
    fs.mkdirSync(outputFolder);
  }

  writeObjects(output.objects, config.write_source_map, outputFolder);
  console.log(output.objects.length + " objects written to disk");

  if (config.write_unit_tests === true) {
    fs.writeFileSync(outputFolder + path.sep + "index.mjs", output.unitTest);
  }
}

run().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});