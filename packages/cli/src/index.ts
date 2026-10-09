import * as fs from "fs";
import * as path from "path";
import ProgressBar from "progress";
import * as Transpiler from "@abaplint/transpiler";
import {TranspilerConfig} from "./config";
import {FileOperations} from "./file_operations";
import {ITranspilerConfig} from "./types";
import {loadLibraries, libraryRegistry} from "./libraries";
import {writeObjects} from "./write_objects";

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

async function build(config: ITranspilerConfig, files: Transpiler.IFile[]) {
  const libraries = await loadLibraries(config);
  const options = {...config.options};
  if (config.write_source_map !== true) {
    // Do not pay to allocate and copy mappings that the CLI will not write.
    options.ignoreSourceMap = true;
  }
  const t = new Transpiler.Transpiler(options);

  const {reg, folders, sources} = libraryRegistry(files, libraries, config.skip_duplicate_dependencies);
  const output = await t.run(reg, new Progress(), folders);
  return {output, sources};
}

async function run() {
  console.log("Transpiler CLI");

  const config = TranspilerConfig.find(process.argv[2]);
  const files = await FileOperations.loadFiles(config);

  console.log("\nBuilding");
  const {output, sources} = await build(config, files);

  console.log("\nOutput");
  const outputFolder = config.output_folder;
  if (!fs.existsSync(outputFolder)) {
    fs.mkdirSync(outputFolder, {recursive: true});
  }

  await writeObjects(output.objects, config, sources);
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
