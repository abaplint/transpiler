import * as fs from "fs";
import * as path from "path";
import * as glob from "glob";
import * as childProcess from "child_process";
import * as os from "os";
import {createRequire} from "module";
import ProgressBar from "progress";
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
  const files: Transpiler.IFile[] = [];

  for (const lib of config.libs || []) {
    let dir = "";
    let cleanupFolder = false;
    if (lib.folder !== undefined && lib.folder !== "" && fs.existsSync(process.cwd() + lib.folder)) {
      console.log("From folder: " + lib.folder);
      dir = process.cwd() + lib.folder;
    } else {
      console.log("Clone: " + lib.url);
      dir = fs.mkdtempSync(path.join(os.tmpdir(), "abap_transpile-"));
      childProcess.execSync("git clone --quiet --depth 1 " + lib.url + " .", {cwd: dir, stdio: "inherit"});
      cleanupFolder = true;
    }

    let patterns = ["/src/**"];
    if (lib.files !== undefined && typeof lib.files === "string" && lib.files !== "") {
      patterns = [lib.files];
    } else if (Array.isArray(lib.files)) {
      patterns = lib.files;
    }

    const excludeFilters = (lib.exclude_filter ?? []).map(pattern => new RegExp(pattern, "i"));

    const filesToRead: string[] = [];
    for (const pattern of patterns) {
      for (const filename of glob.sync(dir + pattern, {nosort: true, nodir: true})) {
        if (filename.endsWith(".clas.testclasses.abap")) {
          continue;
        } else if (excludeFilters.length > 0 && excludeFilters.some(a => a.test(filename)) === true) {
          continue;
        }

        filesToRead.push(filename);
      }
    }
    files.push(...await FileOperations.readAllFiles(filesToRead, ""));

    console.log("\t" + filesToRead.length + " files added from lib");
    if (cleanupFolder === true) {
      FileOperations.deleteFolderRecursive(dir);
    }
  }
  return files;
}

async function writeObjects(outputFiles: Transpiler.IOutputFile[],
  config: ITranspilerConfig, outputFolder: string, files: Transpiler.IFile[]) {

  const writeSourceMaps = config.write_source_map || false;
  const filesToWrite: {path: string, contents: string}[] = [];

  for (const output of outputFiles) {
    const type = output.object.type.toUpperCase();
    let contents = output.chunk.getCode();

    // PROG output gets a runtime bootstrap line prepended, which shifts every
    // generated line down by one - the source map must account for this offset
    let generatedLineOffset = 0;
    if (type === "PROG") {
      // hmm, will this work for INCLUDEs ?
      contents = `if (!globalThis.abap) await import("./_init.mjs");\n` + contents;
      generatedLineOffset = 1;
    }

    if (writeSourceMaps === true
        && (type === "PROG" || type === "FUGR" || type === "CLAS")) {
      const name = output.filename + ".map";
// SourceMappingUrl needs to be percent-encoded, ref https://github.com/microsoft/TypeScript/issues/40951
      contents = contents + `\n//# sourceMappingURL=` + name.replace(/#/g, "%23");

      // map the bare abap filename each mapping carries to its path on disk;
      // source map "sources" are URLs, so always forward slashes, also on Windows
      const sourcePaths: {[filename: string]: string} = {};
      for (const f of files) {
        if (f.relative === undefined) {
          continue;
        }
        const rel = f.relative.split(path.sep).join("/");
        sourcePaths[f.filename] = rel === "" ? f.filename : `${rel}/${f.filename}`;
      }

      const map = output.chunk.getMap(output.filename, {generatedLineOffset, sourcePaths});
      filesToWrite.push({path: outputFolder + path.sep + name, contents: map});
    }

    filesToWrite.push({path: outputFolder + path.sep + output.filename, contents});
  }

  await FileOperations.writeFiles(filesToWrite);
}

const PLUGIN_MODULE = "@abaplint/transpiler-extras";

function loadPlugin(): Transpiler.ITranspilerPlugin | undefined {
  // createRequire so the plugin resolves from the project consuming the CLI, also after webpacking
  const projectRequire = createRequire(path.join(process.cwd(), "package.json"));
  try {
    projectRequire.resolve(PLUGIN_MODULE);
  } catch {
    // plugin package not installed
    return undefined;
  }
  const plugin: Transpiler.ITranspilerPlugin | undefined = projectRequire(PLUGIN_MODULE).plugin;
  if (plugin === undefined) {
    throw new Error(PLUGIN_MODULE + " does not export a plugin");
  }
  console.log("Plugin loaded: " + PLUGIN_MODULE + ", object types: " + plugin.objectTypes().join(", "));
  console.log("Note this requires license for commercial use");
  return plugin;
}

async function build(config: ITranspilerConfig, files: Transpiler.IFile[]) {
  const libFiles = await loadLib(config);
  const t = new Transpiler.Transpiler(config.options, loadPlugin());

  const reg: abaplint.IRegistry = new abaplint.Registry();
  for (const f of files) {
    reg.addFile(new abaplint.MemoryFile(f.filename, f.contents));
  }
  for (const l of libFiles) {
    reg.addDependency(new abaplint.MemoryFile(l.filename, l.contents));
  }
  reg.parse();

  const output = await t.run(reg, new Progress());
  return output;
}

async function run() {
  console.log("Transpiler CLI");

  const config = TranspilerConfig.find(process.argv[2]);
  const files = await FileOperations.loadFiles(config);

  console.log("\nBuilding");
  const output = await build(config, files);

  console.log("\nOutput");
  const outputFolder = config.output_folder;
  if (!fs.existsSync(outputFolder)) {
    fs.mkdirSync(outputFolder);
  }

  await writeObjects(output.objects, config, outputFolder, files);
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
