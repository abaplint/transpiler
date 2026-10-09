import {execFileSync} from "node:child_process";
import {cpSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync} from "node:fs";
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
  const phases: (keyof ITimings)[] = [
    "inputLoadingMs", "libraryLoadingMs", "transpilationMs", "artifactPreparationMs",
    "validationMs", "comparisonMs", "artifactWriteMs", "cleanupMs", "manifestMs", "totalMs",
  ];
  const last = samples[samples.length - 1].timings;
  return {
    runs: samples.length,
    wallMs: {
      median: percentile(samples.map(sample => sample.wallMs), 0.5),
      min: Math.min(...samples.map(sample => sample.wallMs)),
      max: Math.max(...samples.map(sample => sample.wallMs)),
    },
    phaseMedianMs: Object.fromEntries(phases.map(name => [
      name, percentile(samples.map(sample => sample.timings[name] as number), 0.5),
    ])),
    cpuMedianMs: percentile(samples.map(sample => sample.timings.cpuMs), 0.5),
    maxRssBytes: Math.max(...samples.map(sample => sample.timings.maxRssBytes)),
    lastOperations: {
      files: last.files,
      filesRead: last.filesRead,
      fileWrites: last.fileWrites,
      deleted: last.deleted,
      metadataChecks: last.metadataChecks,
      bytesRead: last.bytesRead,
      bytesWritten: last.bytesWritten,
    },
  };
}

function safelyRemoveOutput(work: string) {
  const root = path.resolve(work);
  const output = path.resolve(root, "output");
  if (!output.startsWith(root + path.sep)) {
    throw new Error("Refusing to remove output outside the benchmark workspace: " + output);
  }
  rmSync(output, {recursive: true, force: true});
}

function configure(work: string, dependencyRepo: string, incremental: boolean, maps: boolean) {
  writeFileSync(path.join(work, "abap_transpile.json"), JSON.stringify({
    input_folder: ["src", "deps", "benchmark_overlay/src"],
    exclude_filter: [".*\\.tabl\\.xml$"],
    output_folder: path.join(work, "output"),
    incremental_output: incremental,
    libs: [{url: dependencyRepo, name: "lint_deps"}],
    write_unit_tests: false,
    write_source_map: maps,
    options: {addCommonJS: true, ignoreSyntaxCheck: true, unknownTypes: "runtimeError"},
  }));
}

function build(work: string): ITimings {
  const stdout = execFileSync(process.execPath, [cli], {
    cwd: work,
    encoding: "utf8",
    timeout: 300000,
    env: {...process.env, ABAP_TRANSPILER_TIMING: "1"},
    stdio: ["ignore", "pipe", "pipe"],
  });
  const line = stdout.split(/\r?\n/).find(candidate => candidate.startsWith("ABAP_TRANSPILER_TIMINGS_MS="));
  if (line === undefined) {
    throw new Error("Transpiler timing output was not produced:\n" + stdout);
  }
  return JSON.parse(line.substring("ABAP_TRANSPILER_TIMINGS_MS=".length)) as ITimings;
}

function measure(work: string): ISample {
  const start = performance.now();
  const timings = build(work);
  return {wallMs: performance.now() - start, timings};
}

function toggleGeneratedDeclaration(mainProgram: string) {
  const contents = readFileSync(mainProgram, "utf8");
  const declaration = "DATA benchmark_marker TYPE string.";
  let updated: string;
  if (contents.includes(declaration)) {
    updated = contents.replace(declaration + "\n", "").replace(declaration, "");
  } else {
    const report = /^(REPORT zabapgit LINE-SIZE \d+\.)/m;
    if (!report.test(contents)) {
      throw new Error("Could not find the benchmark source statement in zabapgit.prog.abap");
    }
    updated = contents.replace(report, "$1\n" + declaration);
  }
  writeFileSync(mainProgram, updated);
}

function benchmarkChangedSource(work: string, dependencyRepo: string, maps: boolean) {
  const mode = maps ? "on" : "off";
  configure(work, dependencyRepo, true, maps);
  safelyRemoveOutput(work);
  console.error("[abapGit benchmark] maps=" + mode + " case=incremental-clean seed");
  const clean = measure(work);
  console.error("[abapGit benchmark] maps=" + mode + " case=incremental-semantic-source-change");
  toggleGeneratedDeclaration(path.join(work, "src", "zabapgit.prog.abap"));
  const changed = measure(work);
  return {
    incrementalClean: summarize([clean]),
    incrementalSemanticSourceChange: summarize([changed]),
  };
}

function benchmarkMapMode(work: string, dependencyRepo: string, runs: number, maps: boolean) {
  const mode = maps ? "on" : "off";
  console.error("[abapGit benchmark] maps=" + mode + " case=legacy-existing seed");
  configure(work, dependencyRepo, false, maps);
  build(work);
  const legacy: ISample[] = [];
  const clean: ISample[] = [];
  const incrementalClean: ISample[] = [];
  const unchanged: ISample[] = [];
  const mainProgram = path.join(work, "src", "zabapgit.prog.abap");
  const changed: ISample[] = [];
  const added: ISample[] = [];
  const removed: ISample[] = [];
  const addedFile = path.join(work, "src", "zabapgit_benchmark.prog.abap");
  for (let index = 0; index < runs; index++) {
    configure(work, dependencyRepo, false, maps);
    console.error("[abapGit benchmark] maps=" + mode + " case=legacy-existing sample=" + (index + 1) + "/" + runs);
    legacy.push(measure(work));

    console.error("[abapGit benchmark] maps=" + mode + " case=delete-and-rebuild sample=" + (index + 1) + "/" + runs);
    let start = performance.now();
    safelyRemoveOutput(work);
    let timings = build(work);
    clean.push({wallMs: performance.now() - start, timings});

    configure(work, dependencyRepo, true, maps);
    console.error("[abapGit benchmark] maps=" + mode + " case=incremental-clean sample=" + (index + 1) + "/" + runs);
    start = performance.now();
    safelyRemoveOutput(work);
    timings = build(work);
    incrementalClean.push({wallMs: performance.now() - start, timings});

    console.error("[abapGit benchmark] maps=" + mode + " case=incremental-unchanged sample=" + (index + 1) + "/" + runs);
    unchanged.push(measure(work));

    toggleGeneratedDeclaration(mainProgram);
    console.error("[abapGit benchmark] maps=" + mode + " case=incremental-one-source-change sample=" + (index + 1) + "/" + runs);
    changed.push(measure(work));

    console.error("[abapGit benchmark] maps=" + mode + " case=incremental-added-object sample=" + (index + 1) + "/" + runs);
    writeFileSync(addedFile, "REPORT zabapgit_benchmark.\nWRITE 'benchmark'.\n");
    added.push(measure(work));
    rmSync(addedFile, {force: true});
    console.error("[abapGit benchmark] maps=" + mode + " case=incremental-removed-object sample=" + (index + 1) + "/" + runs);
    removed.push(measure(work));
  }
  return {
    "legacy-existing": summarize(legacy),
    "delete-and-rebuild": summarize(clean),
    "incremental-clean-output": summarize(incrementalClean),
    "incremental-unchanged": summarize(unchanged),
    "incremental-one-source-change": summarize(changed),
    "incremental-added-object": summarize(added),
    "incremental-removed-object": summarize(removed),
  };
}

function validateInputs(project: string, dependencyRepo: string, overlay: string) {
  for (const [label, folder] of [["project", project], ["dependency repository", dependencyRepo], ["overlay", overlay]]) {
    if (!path.isAbsolute(folder)) {
      throw new Error("The " + label + " path must resolve to an absolute path");
    }
  }
}

async function main() {
  const project = path.resolve(process.argv[2] || "");
  const dependencyRepo = path.resolve(process.argv[3] || "");
  const overlay = path.resolve(process.argv[4] || "");
  const runs = Math.max(1, Number(process.argv[5]) || 3);
  const mapMode = process.argv[6] || "both";
  if (process.argv.length < 5) {
    throw new Error("Usage: node build/performance/transpiler_project.js <project> <dependency-git-repo> <overlay> [runs] [both|on|off|change]");
  }
  if (!["both", "on", "off", "change"].includes(mapMode)) {
    throw new Error("Source-map mode must be both, on, off, or change");
  }
  validateInputs(project, dependencyRepo, overlay);

  const tempRoot = mkdtempSync(path.join(os.tmpdir(), "abaplint-project-benchmark-"));
  const work = path.join(tempRoot, "project");
  try {
    mkdirSync(work);
    cpSync(path.join(project, "src"), path.join(work, "src"), {recursive: true});
    cpSync(path.join(project, "deps"), path.join(work, "deps"), {recursive: true});
    cpSync(path.join(overlay, "src"), path.join(work, "benchmark_overlay", "src"), {recursive: true});
    const results = {
      project: "abapGit",
      runs,
      sources: "full src plus bundled deps; excludes 42 SAP table XML files that the transpiler database schema cannot resolve",
      adapters: "three benchmark-only SAP GUI class/event declarations copied outside the upstream checkout",
      library: "local clone of abaplint/deps; contents held fixed across runs",
      filesystemCache: "normal OS cache",
      ...(mapMode === "both" || mapMode === "on"
        ? {mapsEnabled: benchmarkMapMode(work, dependencyRepo, runs, true)}
        : {}),
      ...(mapMode === "both" || mapMode === "off"
        ? {mapsDisabled: benchmarkMapMode(work, dependencyRepo, runs, false)}
        : {}),
      ...(mapMode === "change"
        ? {mapsDisabled: benchmarkChangedSource(work, dependencyRepo, false)}
        : {}),
    };
    console.log(JSON.stringify(results, null, 2));
  } finally {
    const temp = path.resolve(tempRoot);
    const expected = path.resolve(os.tmpdir()) + path.sep;
    if (!temp.startsWith(expected) || !path.basename(temp).startsWith("abaplint-project-benchmark-")) {
      throw new Error("Refusing to remove unexpected benchmark temp directory: " + temp);
    }
    rmSync(temp, {recursive: true, force: true});
  }
}

if (require.main === module) {
  main().catch(error => {
    console.error(error);
    process.exitCode = 1;
  });
}
