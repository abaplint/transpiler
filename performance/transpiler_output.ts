import {execFileSync} from "node:child_process";
import {copyFileSync, mkdirSync, mkdtempSync, rmSync, writeFileSync} from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import {performance} from "node:perf_hooks";

interface ITimings {
  inputLoadingMs: number;
  libraryLoadingMs: number;
  transpilationMs: number;
  artifactPreparationMs: number;
  validationMs: number;
  comparisonMs: number;
  artifactWriteMs: number;
  cleanupMs: number;
  manifestMs: number;
  totalMs: number;
  files: number;
  filesRead: number;
  fileWrites: number;
  deleted: number;
  metadataChecks: number;
  bytesRead: number;
  bytesWritten: number;
  cpuMs: number;
  maxRssBytes: number;
}

interface ISample {
  wallMs: number;
  timings: ITimings;
}

const cli = path.resolve("packages/cli/build/bundle.js");

function percentile(values: number[], fraction: number): number {
  const ordered = [...values].sort((a, b) => a - b);
  return ordered[Math.min(ordered.length - 1, Math.floor(ordered.length * fraction))];
}

function summarize(samples: ISample[]) {
  const phaseNames: (keyof ITimings)[] = [
    "inputLoadingMs", "libraryLoadingMs", "transpilationMs", "artifactPreparationMs",
    "validationMs", "comparisonMs", "artifactWriteMs", "cleanupMs", "manifestMs", "totalMs",
  ];
  return {
    runs: samples.length,
    wallMs: {
      median: percentile(samples.map(sample => sample.wallMs), 0.5),
      min: Math.min(...samples.map(sample => sample.wallMs)),
      max: Math.max(...samples.map(sample => sample.wallMs)),
    },
    phaseMedianMs: Object.fromEntries(phaseNames.map(name => [
      name, percentile(samples.map(sample => sample.timings[name] as number), 0.5),
    ])),
    cpuMedianMs: percentile(samples.map(sample => sample.timings.cpuMs), 0.5),
    maxRssBytes: Math.max(...samples.map(sample => sample.timings.maxRssBytes)),
    lastOperations: {
      files: samples[samples.length - 1].timings.files,
      filesRead: samples[samples.length - 1].timings.filesRead,
      fileWrites: samples[samples.length - 1].timings.fileWrites,
      deleted: samples[samples.length - 1].timings.deleted,
      metadataChecks: samples[samples.length - 1].timings.metadataChecks,
      bytesRead: samples[samples.length - 1].timings.bytesRead,
      bytesWritten: samples[samples.length - 1].timings.bytesWritten,
    },
  };
}

function configure(folder: string, incremental: boolean, maps: boolean, clonedLibrary?: string) {
  writeFileSync(path.join(folder, "abap_transpile.json"), JSON.stringify({
    input_folder: "src",
    output_folder: "output",
    incremental_output: incremental,
    libs: clonedLibrary === undefined
      ? [{folder: "fixed-library", name: "benchmark_library"}]
      : [{url: clonedLibrary, name: "benchmark_library"}],
    write_unit_tests: false,
    write_source_map: maps,
    options: {addCommonJS: true},
  }));
}

function build(folder: string): ITimings {
  const stdout = execFileSync(process.execPath, [cli], {
    cwd: folder,
    encoding: "utf8",
    timeout: 180000,
    env: {...process.env, ABAP_TRANSPILER_TIMING: "1"},
    stdio: ["ignore", "pipe", "pipe"],
  });
  const line = stdout.split(/\r?\n/).find(candidate => candidate.startsWith("ABAP_TRANSPILER_TIMINGS_MS="));
  if (line === undefined) {
    throw new Error("Transpiler timing output was not produced:\n" + stdout);
  }
  return JSON.parse(line.substring("ABAP_TRANSPILER_TIMINGS_MS=".length)) as ITimings;
}

function measure(folder: string): ISample {
  const start = performance.now();
  const timings = build(folder);
  return {wallMs: performance.now() - start, timings};
}

function makeProject(folder: string, count: number) {
  mkdirSync(path.join(folder, "src"), {recursive: true});
  const statements = Array.from({length: 12}, () => "ASSERT 1 = 1.").join("\n") + "\n";
  for (let index = 0; index < count; index++) {
    const name = "zbench" + index.toString().padStart(5, "0") + ".prog.abap";
    writeFileSync(path.join(folder, "src", name), statements);
  }
  writeFileSync(path.join(folder, "src", "zbenchmark.w3mi.xml"), [
    '<abapGit><asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0"><asx:values>',
    '<NAME>ZBENCHMARK.BIN</NAME><TEXT>synthetic benchmark asset</TEXT><PARAMS/></asx:values></asx:abap></abapGit>',
  ].join(""));
  writeFileSync(path.join(folder, "src", "zbenchmark.w3mi.data.bin"), Buffer.alloc(2 * 1024 * 1024, 0xA5));

  const library = path.join(folder, "fixed-library", "src");
  mkdirSync(library, {recursive: true});
  for (const filename of [
    "cl_abap_char_utilities.clas.abap",
    "zif_abapgit_definitions.intf.abap",
    "zcl_client.clas.abap",
  ]) {
    copyFileSync(path.resolve("unit-test/test-9", filename), path.join(library, filename));
  }
}

function makeClonedLibrary(folder: string): string {
  const repository = path.join(folder, "cloned-library-repo");
  const source = path.join(repository, "src");
  mkdirSync(source, {recursive: true});
  for (const filename of [
    "cl_abap_char_utilities.clas.abap",
    "zif_abapgit_definitions.intf.abap",
    "zcl_client.clas.abap",
  ]) {
    copyFileSync(path.resolve("unit-test/test-9", filename), path.join(source, filename));
  }
  execFileSync("git", ["init", "--quiet"], {cwd: repository, stdio: "pipe"});
  execFileSync("git", ["add", "--all"], {cwd: repository, stdio: "pipe"});
  execFileSync("git", [
    "-c", "user.name=Benchmark fixture", "-c", "user.email=benchmark@example.invalid",
    "commit", "--quiet", "-m", "fixed benchmark library",
  ], {cwd: repository, stdio: "pipe"});
  return repository;
}

function benchmarkMapMode(folder: string, runs: number, maps: boolean) {
  const results: Record<string, unknown> = {};

  configure(folder, false, maps);
  build(folder);
  results["legacy-existing"] = summarize(Array.from({length: runs}, () => measure(folder)));

  const clean: ISample[] = [];
  for (let index = 0; index < runs; index++) {
    const start = performance.now();
    rmSync(path.join(folder, "output"), {recursive: true, force: true});
    const timings = build(folder);
    clean.push({wallMs: performance.now() - start, timings});
  }
  results["delete-and-rebuild"] = summarize(clean);

  configure(folder, true, maps);
  const incrementalClean: ISample[] = [];
  for (let index = 0; index < runs; index++) {
    const start = performance.now();
    rmSync(path.join(folder, "output"), {recursive: true, force: true});
    const timings = build(folder);
    incrementalClean.push({wallMs: performance.now() - start, timings});
  }
  results["incremental-clean-output"] = summarize(incrementalClean);

  results["incremental-unchanged"] = summarize(Array.from({length: runs}, () => measure(folder)));

  const changed: ISample[] = [];
  const changedFile = path.join(folder, "src", "zbench00000.prog.abap");
  let value = 2;
  for (let index = 0; index < runs; index++) {
    writeFileSync(changedFile, `ASSERT 1 = ${value}.\n`);
    value = value === 2 ? 3 : 2;
    changed.push(measure(folder));
  }
  results["incremental-one-change"] = summarize(changed);

  const added: ISample[] = [];
  const removed: ISample[] = [];
  const addedFile = path.join(folder, "src", "zbench_added.prog.abap");
  for (let index = 0; index < runs; index++) {
    writeFileSync(addedFile, "ASSERT 1 = 1.\n");
    added.push(measure(folder));
    rmSync(addedFile, {force: true});
    removed.push(measure(folder));
  }
  results["incremental-added-object"] = summarize(added);
  results["incremental-removed-object"] = summarize(removed);

  return results;
}

function benchmarkClonedLibrary(folder: string, runs: number, repository: string) {
  configure(folder, true, true, repository);
  build(folder);
  return summarize(Array.from({length: runs}, () => measure(folder)));
}

async function main() {
  const count = Math.max(10, Number(process.argv[2]) || 300);
  const runs = Math.max(3, Number(process.argv[3]) || 5);
  const folder = mkdtempSync(path.join(os.tmpdir(), "abaplint-transpiler-benchmark-"));
  try {
    makeProject(folder, count);
    const clonedLibrary = makeClonedLibrary(folder);
    const results = {
      sourcePrograms: count,
      runs,
      filesystemCache: "normal OS cache; first build excluded from timed repeated cases",
      libraries: "fixed local unit-test fixture for the main cases",
      mapsEnabled: benchmarkMapMode(folder, runs, true),
      mapsDisabled: benchmarkMapMode(folder, runs, false),
      clonedLibraryStableContents: benchmarkClonedLibrary(folder, runs, clonedLibrary),
    };
    console.log(JSON.stringify(results, null, 2));
  } finally {
    rmSync(folder, {recursive: true, force: true});
  }
}

if (require.main === module) {
  main().catch(error => {
    console.error(error);
    process.exitCode = 1;
  });
}
