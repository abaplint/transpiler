import * as fs from "fs";
import * as path from "path";
import * as glob from "glob";
import * as childProcess from "child_process";
import * as os from "os";
import * as ProgressBar from "progress";
import * as Transpiler from "@abaplint/transpiler";
import * as abaplint from "@abaplint/core";
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
  if (config.lib && config.lib !== "" && config.libs === undefined) {
    config.libs = [{url: config.lib}];
  }

  for (const l of config.libs || []) {
    console.log("Clone: " + l.url);
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "abap_transpile-"));
    childProcess.execSync("git clone --quiet --depth 1 " + l.url + " .", {cwd: dir, stdio: "inherit"});
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

function writeObjects(objects: Transpiler.IOutputFile[], writeSourceMaps: boolean, outputFolder: string, files: Transpiler.IFile[]) {
  for (const o of objects) {
    let contents = o.chunk.getCode();
    if (writeSourceMaps === true) {
      const name = o.filename + ".map";
      contents = contents + `\n//# sourceMappingURL=` + name;
      let map = o.chunk.getMap(o.filename);
      for (const f of files) { // hack the paths to the original files
        if (f.relative === undefined) {
          continue;
        }
        if (map.includes(`"${f.filename}"`)) {
          let withPath = `"${f.relative}${path.sep}${f.filename}"`;
          withPath = withPath.replace(/\\/g, "\\\\");
          map = map.replace(`"${f.filename}"`, withPath);
        }
      }
      fs.writeFileSync(outputFolder + path.sep + name, map);
    }
    fs.writeFileSync(outputFolder + path.sep + o.filename, contents);
  }
}

async function run() {
  console.log("Transpiler CLI");

  const config = TranspilerConfig.find(process.argv[2]);
  const libFiles = loadLib(config);
  const files = FileOperations.loadFiles(config);

  console.log("\nBuilding");
  const t = new Transpiler.Transpiler(config.options);

  const reg: abaplint.IRegistry = new abaplint.Registry();
  for (const f of files) {
    reg.addFile(new abaplint.MemoryFile(f.filename, f.contents));
  }
  for (const l of libFiles) {
    reg.addDependency(new abaplint.MemoryFile(l.filename, l.contents));
  }
  reg.parse();

  const output = await t.run(reg, new Progress());

  console.log("\nOutput");
  const outputFolder = config.output_folder;
  if (!fs.existsSync(outputFolder)) {
    fs.mkdirSync(outputFolder);
  }

  writeObjects(output.objects, config.write_source_map, outputFolder, files);
  console.log(output.objects.length + " objects written to disk");

  if (config.write_unit_tests === true) {
    fs.writeFileSync(outputFolder + path.sep + "index.mjs", output.unitTestScript);
  }
  fs.writeFileSync(outputFolder + path.sep + "init.mjs", output.initializationScript);
}

run().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});