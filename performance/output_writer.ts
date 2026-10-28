import {mkdtempSync, rmSync} from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import {performance} from "node:perf_hooks";
import {IOutputArtifact, IOutputWriteResult, writeOutput} from "../packages/cli/src/output_writer";

interface ISample {
  milliseconds: number;
  cpuMilliseconds: number;
  rssBytes: number;
  result: IOutputWriteResult;
}

function makeArtifacts(count: number, assetBytes: number): IOutputArtifact[] {
  const artifacts: IOutputArtifact[] = [];
  for (let index = 0; index < count; index++) {
    const name = "project/zobject" + index.toString().padStart(6, "0") + ".clas.mjs";
    artifacts.push({path: name, contents: `export const value${index} = ${index};\n`});
    artifacts.push({path: name + ".map", contents: JSON.stringify({
      version: 3, file: path.posix.basename(name), sources: ["src/zobject" + index + ".clas.abap"],
      sourcesContent: ["CLASS zobject" + index + " DEFINITION PUBLIC. ENDCLASS."], names: [], mappings: "",
    })});
  }
  if (assetBytes > 0) {
    const contents = Buffer.alloc(assetBytes, 0xA5).toString("latin1");
    artifacts.push({path: "project/assets.w3mi.data.bin", contents});
  }
  return artifacts;
}

function percentile(values: number[], fraction: number): number {
  const ordered = [...values].sort((a, b) => a - b);
  return ordered[Math.min(ordered.length - 1, Math.floor(ordered.length * fraction))];
}

function summarize(samples: ISample[]) {
  const times = samples.map(sample => sample.milliseconds);
  return {
    runs: samples.length,
    medianMs: percentile(times, 0.5),
    p90Ms: percentile(times, 0.9),
    minMs: Math.min(...times),
    maxMs: Math.max(...times),
    cpuMsMedian: percentile(samples.map(sample => sample.cpuMilliseconds), 0.5),
    rssBytesMax: Math.max(...samples.map(sample => sample.rssBytes)),
    lastResult: samples[samples.length - 1].result,
  };
}

async function measure(operation: () => Promise<IOutputWriteResult>): Promise<ISample> {
  (globalThis as typeof globalThis & {gc?: () => void}).gc?.();
  const cpuBefore = process.cpuUsage();
  let peakRss = process.memoryUsage.rss();
  const monitor = setInterval(() => {
    peakRss = Math.max(peakRss, process.memoryUsage.rss());
  }, 2);
  const start = performance.now();
  let result: IOutputWriteResult;
  let milliseconds: number;
  let cpu: NodeJS.CpuUsage;
  try {
    result = await operation();
    milliseconds = performance.now() - start;
    cpu = process.cpuUsage(cpuBefore);
  } finally {
    clearInterval(monitor);
  }
  peakRss = Math.max(peakRss, process.memoryUsage.rss());
  return {
    milliseconds,
    cpuMilliseconds: (cpu.user + cpu.system) / 1000,
    rssBytes: peakRss,
    result,
  };
}

async function benchmarkMode(
  mode: "legacy-existing" | "delete-and-rebuild" | "incremental-unchanged"
    | "incremental-one-change" | "incremental-size-change",
  artifacts: IOutputArtifact[],
  runs: number,
  ioConcurrency: number,
): Promise<ISample[]> {
  const folder = mkdtempSync(path.join(os.tmpdir(), "abaplint-output-benchmark-"));
  const output = path.join(folder, "output");
  try {
    await writeOutput(output, artifacts, mode.startsWith("incremental"), ioConcurrency);
    if (mode !== "delete-and-rebuild") {
      await writeOutput(output, artifacts, mode.startsWith("incremental"), ioConcurrency);
    }
    const samples: ISample[] = [];
    for (let iteration = 0; iteration < runs; iteration++) {
      if (mode.startsWith("incremental-") && mode !== "incremental-unchanged") {
        await writeOutput(output, artifacts, true, ioConcurrency);
      }
      const runArtifacts = mode === "incremental-one-change"
        ? artifacts.map((file, index) => index === 0 ? {...file, contents: file.contents.replace("value0", "valuE0")} : file)
        : mode === "incremental-size-change"
          ? artifacts.map((file, index) => index === 0 ? {...file, contents: file.contents + " "} : file)
          : artifacts;
      samples.push(await measure(async () => {
        if (mode === "delete-and-rebuild") {
          rmSync(output, {recursive: true, force: true});
        }
        return writeOutput(output, runArtifacts, mode.startsWith("incremental"), ioConcurrency);
      }));
    }
    return samples;
  } finally {
    rmSync(folder, {recursive: true, force: true});
  }
}

async function main() {
  const count = Math.max(1, Number(process.argv[2]) || 1000);
  const runs = Math.max(3, Number(process.argv[3]) || 7);
  const assetBytes = 8 * 1024 * 1024;
  const artifacts = makeArtifacts(count, assetBytes);
  const results: Record<string, unknown> = {};
  results["legacy-existing"] = summarize(await benchmarkMode("legacy-existing", artifacts, runs, 8));
  results["delete-and-rebuild"] = summarize(await benchmarkMode("delete-and-rebuild", artifacts, runs, 8));
  for (const ioConcurrency of [1, 4, 8]) {
    for (const mode of ["incremental-unchanged", "incremental-one-change", "incremental-size-change"] as const) {
      results[mode + "-io-" + ioConcurrency] =
        summarize(await benchmarkMode(mode, artifacts, runs, ioConcurrency));
    }
  }
  console.log(JSON.stringify({
    count,
    runs,
    assetBytes,
    artifactCount: artifacts.length,
    filesystemCache: "normal OS cache; first setup is excluded from timed runs",
    concurrency: "internal I/O limit tested at 1, 4, and 8",
    results,
  }, null, 2));
}

if (require.main === module) {
  main().catch(error => {
    console.error(error);
    process.exitCode = 1;
  });
}
