import ProgressBar from "progress";
import * as Transpiler from "@abaplint/transpiler";
import {TranspilerConfig} from "./config";
import {FileOperations} from "./file_operations";
import {ITranspilerConfig} from "./types";
import {loadLibraries, libraryRegistry} from "./libraries";
import {collectObjectFiles} from "./write_objects";
import {IOutputArtifact, writeOutput} from "./output_writer";

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
  const artifacts: IOutputArtifact[] = collectObjectFiles(output.objects, config, sources);
  if (config.write_unit_tests === true) {
    artifacts.push({path: "index.mjs", contents: output.unitTestScript});
    artifacts.push({path: "_unit_open.mjs", contents: output.unitTestScriptOpen});
  }
  artifacts.push({path: "init.mjs", contents: output.initializationScript});
  artifacts.push({path: "_init.mjs", contents: output.initializationScript2});
  artifacts.push({path: "_top.mjs", contents: `import runtime from "@abaplint/runtime";
globalThis.abap = new runtime.ABAP();`});
  const written = await writeOutput(config.output_folder, artifacts, config.incremental_output === true);
  if (config.incremental_output === true) {
    console.log("Output files: " + written.created + " created, " +
      written.updated + " updated, " + written.unchanged + " unchanged, " + written.deleted + " deleted");
  } else {
    console.log(`Output files: ${written.updated} written`);
  }
}

run().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});
