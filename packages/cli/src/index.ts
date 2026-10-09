import ProgressBar from "progress";
import * as Transpiler from "@abaplint/transpiler";
import {performance} from "node:perf_hooks";
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
  const libraryStart = performance.now();
  const libraries = await loadLibraries(config);
  const libraryLoadingMs = performance.now() - libraryStart;
  const options = {...config.options};
  if (config.write_source_map !== true) {
    // Do not pay to allocate and copy mappings that the CLI will not write.
    options.ignoreSourceMap = true;
  }
  const t = new Transpiler.Transpiler(options);

  const {reg, folders, sources, sourceMapPaths} = libraryRegistry(files, libraries);
  const transpileStart = performance.now();
  const output = await t.run(reg, new Progress(), folders);
  const transpilationMs = performance.now() - transpileStart;
  return {output, sources, sourceMapPaths, libraryLoadingMs, transpilationMs};
}

async function run() {
  const totalStart = performance.now();
  const cpuStart = process.cpuUsage();
  console.log("Transpiler CLI");

  const inputStart = performance.now();
  const config = TranspilerConfig.find(process.argv[2]);
  const files = await FileOperations.loadFiles(config);
  const inputLoadingMs = performance.now() - inputStart;

  console.log("\nBuilding");
  const {output, sources, sourceMapPaths, libraryLoadingMs, transpilationMs} = await build(config, files);

  console.log("\nOutput");
  const artifactPreparationStart = performance.now();
  const artifacts: IOutputArtifact[] = collectObjectFiles(output.objects, config, sources, sourceMapPaths);
  if (config.write_unit_tests === true) {
    artifacts.push({path: "index.mjs", contents: output.unitTestScript});
    artifacts.push({path: "_unit_open.mjs", contents: output.unitTestScriptOpen});
  }
  artifacts.push({path: "init.mjs", contents: output.initializationScript});
  artifacts.push({path: "_init.mjs", contents: output.initializationScript2});
  artifacts.push({path: "_top.mjs", contents: `import runtime from "@abaplint/runtime";
globalThis.abap = new runtime.ABAP();`});
  const artifactPreparationMs = performance.now() - artifactPreparationStart;
  const written = await writeOutput(config.output_folder, artifacts, config.incremental_output === true);
  if (config.incremental_output === true) {
    console.log("Output files: " + written.created + " created, " +
      written.updated + " updated, " + written.unchanged + " unchanged, " + written.deleted + " deleted");
  } else {
    console.log(`Output files: ${written.fileWrites} written`);
  }
  if (process.env.ABAP_TRANSPILER_TIMING === "1") {
    console.log("ABAP_TRANSPILER_TIMINGS_MS=" + JSON.stringify({
      inputLoadingMs,
      libraryLoadingMs,
      transpilationMs,
      artifactPreparationMs,
      validationMs: written.validationMs,
      comparisonMs: written.comparisonMs,
      artifactWriteMs: written.artifactWriteMs,
      cleanupMs: written.cleanupMs,
      manifestMs: written.manifestMs,
      totalMs: performance.now() - totalStart,
      files: artifacts.length,
      filesRead: written.filesRead,
      fileWrites: written.fileWrites,
      deleted: written.deleted,
      metadataChecks: written.metadataChecks,
      bytesRead: written.bytesRead,
      bytesWritten: written.bytesWritten,
      cpuMs: (() => {
        const cpu = process.cpuUsage(cpuStart);
        return (cpu.user + cpu.system) / 1000;
      })(),
      maxRssBytes: process.resourceUsage().maxRSS * 1024,
    }));
  }
}

run().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});
