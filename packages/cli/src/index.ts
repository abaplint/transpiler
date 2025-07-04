import * as fs from "fs";
import * as path from "path";
import * as glob from "glob";
import * as childProcess from "child_process";
import * as os from "os";
import * as ProgressBar from "progress";
import * as Transpiler from "@abaplint/transpiler";
import * as abaplint from "@abaplint/core";
import {TranspilerConfig} from "./config";
import {FileOperations} from "./file_operations";
import {ITranspilerConfig} from "./types";

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

async function loadLib(config: ITranspilerConfig): Promise<Transpiler.IFile[]> {
  let files: Transpiler.IFile[] = [];
  if (config.lib && config.lib !== "" && config.libs === undefined) {
    config.libs = [{url: config.lib}];
  }

  for (const l of config.libs || []) {
    let dir = "";
    let cleanupFolder = false;
    if (l.folder !== undefined && l.folder !== "" && fs.existsSync(process.cwd() + l.folder)) {
      console.log("From folder: " + l.folder);
      dir = process.cwd() + l.folder;
    } else {
      console.log("Clone: " + l.url);
      dir = fs.mkdtempSync(path.join(os.tmpdir(), "abap_transpile-"));
      childProcess.execSync("git clone --quiet --depth 1 " + l.url + " .", {cwd: dir, stdio: "inherit"});
      cleanupFolder = true;
    }

    let patterns = ["/src/**"];
    if (l.files !== undefined && typeof l.files === "string" && l.files !== "") {
      patterns = [l.files];
    } else if (Array.isArray(l.files)) {
      patterns = l.files;
    }

    const filesToRead: string[] = [];
    for (const pattern of patterns) {
      for (const filename of glob.sync(dir + pattern, {nosort: true, nodir: true})) {
        if (filename.endsWith(".clas.testclasses.abap")) {
          continue;
        }
        filesToRead.push(filename);
        /*
        const contents = fs.readFileSync(filename, "utf8");
        filename = path.basename(filename);
        files.push({filename, contents});
        count++;
        */
      }
    }
    files = await FileOperations.readAllFiles(filesToRead, "");

    console.log("\t" + filesToRead.length + " files added from lib");
    if (cleanupFolder === true) {
      FileOperations.deleteFolderRecursive(dir);
    }
  }
  return files;
}

function writeObjects(objects: Transpiler.IOutputFile[], config: ITranspilerConfig, outputFolder: string, files: Transpiler.IFile[]) {
  const writeSourceMaps = config.write_source_map || false
  for (const o of objects) {
    let contents = o.chunk.getCode();
    if (writeSourceMaps === true
        && o.object.type.toUpperCase() !== "TABL"
        && o.object.type.toUpperCase() !== "DTEL"
        && o.object.type.toUpperCase() !== "W3MI"
        && o.object.type.toUpperCase() !== "SMIM"
        && o.object.type.toUpperCase() !== "ENQU"
        && o.object.type.toUpperCase() !== "TTYP") {
      const name = o.filename + ".map";
// SourceMappingUrl needs to be percent-encoded, ref https://github.com/microsoft/TypeScript/issues/40951
      contents = contents + `\n//# sourceMappingURL=` + name.replace(/#/g, "%23");
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

    if (o.object.type.toUpperCase() === "PROG") {
      // hmm, will this work for INCLUDEs ?
      contents = `if (!globalThis.abap) await import("./_init.mjs");\n` + contents;
    }
    fs.writeFileSync(outputFolder + path.sep + o.filename, contents);
  }
}

async function run() {
  console.log("Transpiler CLI");

  const config = TranspilerConfig.find(process.argv[2]);
  const libFiles = await loadLib(config);
  const files = await FileOperations.loadFiles(config);

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

  writeObjects(output.objects, config, outputFolder, files);
  console.log(output.objects.length + " objects written to disk");

  if (config.write_unit_tests === true) {
    // breaking change? rename this output file,
    fs.writeFileSync(outputFolder + path.sep + "index.mjs", output.unitTestScript);
    fs.writeFileSync(outputFolder + path.sep + "_unit_open.mjs", output.unitTestScriptOpen);
  }
  // breaking change? rename this output file,
  fs.writeFileSync(outputFolder + path.sep + "init.mjs", output.initializationScript);

// new static referenced imports,
  fs.writeFileSync(outputFolder + path.sep + "_init.mjs", output.initializationScript2);
  fs.writeFileSync(outputFolder + path.sep + "_top.mjs", `import runtime from "@abaplint/runtime";
globalThis.abap = new runtime.ABAP();`);
}

run().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});