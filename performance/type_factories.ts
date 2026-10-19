import * as abaplint from "@abaplint/core";
import {ABAP, MemoryConsole} from "../packages/runtime/src";
import {Transpiler} from "../packages/transpiler/src";
import {ITranspilerOptions} from "../packages/transpiler/src/types";
import {performance} from "node:perf_hooks";
import {spawnSync} from "node:child_process";
import {mkdtempSync, rmSync, writeFileSync} from "node:fs";
import {tmpdir} from "node:os";
import {join, resolve} from "node:path";
import {createRequire} from "node:module";
import {pathToFileURL} from "node:url";

const AsyncFunction = Object.getPrototypeOf(async () => {}).constructor as new (...args: string[]) => (...args: any[]) => Promise<void>;
const repetitions = Math.max(10, Number(process.argv[2]) || 100);
const runs = Math.max(3, Number(process.argv[3]) || 7);
const coldStartRuns = Math.max(3, Number(process.argv[4]) || 5);

function makeFixture(): string {
  const leafFields = Array.from({length: 24}, (_, i) => `  field_${i.toString().padStart(2, "0")} TYPE c LENGTH 40`).join(",\n");
  const nodeFields = Array.from({length: 10}, (_, i) => `  leaf_${i.toString().padStart(2, "0")} TYPE ty_leaf`).join(",\n");
  const rootFields = Array.from({length: 6}, (_, i) => `  node_${i.toString().padStart(2, "0")} TYPE ty_node`).join(",\n");
  const declarations = Array.from({length: repetitions}, (_, i) => `DATA item_${i.toString().padStart(4, "0")} TYPE ty_root.`).join("\n");
  return `TYPES: BEGIN OF ty_leaf,\n${leafFields},\nEND OF ty_leaf.\n` +
    `TYPES: BEGIN OF ty_node,\n${nodeFields},\nEND OF ty_node.\n` +
    `TYPES: BEGIN OF ty_root,\n${rootFields},\nEND OF ty_root.\n` + declarations +
    `CLASS lcl_factory DEFINITION.\n` +
    `  PUBLIC SECTION.\n` +
    `    DATA attribute TYPE ty_root.\n` +
    `    METHODS copy IMPORTING input TYPE ty_root OPTIONAL RETURNING VALUE(result) TYPE ty_root.\n` +
    `ENDCLASS.\n` +
    `CLASS lcl_factory IMPLEMENTATION.\n` +
    `  METHOD copy. result = input. ENDMETHOD.\n` +
    `ENDCLASS.\n` +
    `DATA instance TYPE REF TO lcl_factory.\n` +
    `CREATE OBJECT instance.\n` +
    `DATA returned TYPE ty_root.\n` +
    `returned = instance->copy( input = item_0000 ).\n` +
    `DATA constructed TYPE ty_root.\n` +
    `constructed = VALUE ty_root( node_00 = VALUE ty_node( leaf_00 = VALUE ty_leaf( field_00 = 'x' ) ) ).\n` +
    `DATA mapped TYPE ty_root.\n` +
    `mapped = CORRESPONDING ty_root( constructed ).\n`;
}

function percentile(values: number[], fraction: number): number {
  const sorted = [...values].sort((a, b) => a - b);
  return sorted[Math.min(sorted.length - 1, Math.floor(sorted.length * fraction))];
}

function summarize(values: number[]) {
  return {
    medianMs: percentile(values, 0.5),
    p90Ms: percentile(values, 0.9),
    minMs: Math.min(...values),
    maxMs: Math.max(...values),
  };
}

async function sample(source: string, options: ITranspilerOptions) {
  const memory = new abaplint.MemoryFile("ztype_factory_benchmark.prog.abap", source);
  const registry = new abaplint.Registry().addFile(memory);

  const parseStart = performance.now();
  registry.parse();
  const parseMs = performance.now() - parseStart;

  const cpuBefore = process.cpuUsage();
  const transpileStart = performance.now();
  const result = await new Transpiler(options).run(registry);
  const transpileMs = performance.now() - transpileStart;
  const cpu = process.cpuUsage(cpuBefore);
  const code = result.objects[0]?.chunk.getCode() ?? "";

  const compileStart = performance.now();
  const run = new AsyncFunction("abap", code);
  const compileMs = performance.now() - compileStart;

  const abap = new ABAP({console: new MemoryConsole()});
  const runtimeStart = performance.now();
  await run(abap);
  const runtimeMs = performance.now() - runtimeStart;

  return {
    parseMs,
    transpileMs,
    cpuMs: (cpu.user + cpu.system) / 1000,
    compileMs,
    runtimeMs,
    rssBytes: process.memoryUsage.rss(),
    outputBytes: Buffer.byteLength(code, "utf8"),
    factoryCount: code.match(/function \$t_/g)?.length ?? 0,
  };
}

async function measureComparison(source: string) {
  const samples: {inline: Awaited<ReturnType<typeof sample>>[], shared: Awaited<ReturnType<typeof sample>>[]} = {
    inline: [],
    shared: [],
  };
  const collect = async (sharedTypeFactories: boolean, target: typeof samples.inline) => {
    (globalThis as typeof globalThis & {gc?: () => void}).gc?.();
    target.push(await sample(source, {sharedTypeFactories}));
  };

  for (let warmup = 0; warmup < 2; warmup++) {
    if (warmup % 2 === 0) {
      await collect(false, samples.inline);
      await collect(true, samples.shared);
    } else {
      await collect(true, samples.shared);
      await collect(false, samples.inline);
    }
  }
  samples.inline.length = 0;
  samples.shared.length = 0;

  for (let run = 0; run < runs; run++) {
    if (run % 2 === 0) {
      await collect(false, samples.inline);
      await collect(true, samples.shared);
    } else {
      await collect(true, samples.shared);
      await collect(false, samples.inline);
    }
  }

  const summarizeMode = (modeSamples: typeof samples.inline) => ({
    outputBytes: modeSamples[modeSamples.length - 1].outputBytes,
    factoryCount: modeSamples[modeSamples.length - 1].factoryCount,
    rssBytesMedian: percentile(modeSamples.map(s => s.rssBytes), 0.5),
    parse: summarize(modeSamples.map(s => s.parseMs)),
    transpile: summarize(modeSamples.map(s => s.transpileMs)),
    cpu: summarize(modeSamples.map(s => s.cpuMs)),
    javascriptCompile: summarize(modeSamples.map(s => s.compileMs)),
    runtimeConstruction: summarize(modeSamples.map(s => s.runtimeMs)),
  });
  const inline = summarizeMode(samples.inline);
  const shared = summarizeMode(samples.shared);
  return {
    inline,
    shared,
    outputReductionPercent: Number((100 * (inline.outputBytes - shared.outputBytes) / inline.outputBytes).toFixed(2)),
  };
}

async function buildCode(source: string, sharedTypeFactories: boolean): Promise<string> {
  const registry = new abaplint.Registry().addFile(new abaplint.MemoryFile("ztype_factory_benchmark.prog.abap", source));
  const result = await new Transpiler({sharedTypeFactories}).run(registry);
  return result.objects[0]?.chunk.getCode() ?? "";
}

async function measureArtifacts(source: string) {
  const cliRequire = createRequire(resolve(process.cwd(), "packages/cli/package.json"));
  const terser = cliRequire("terser") as {
    minify: (code: string, options?: {module?: boolean}) => Promise<{code?: string}>;
  };
  const terserPackage = cliRequire("terser/package.json") as {version: string};
  const inlineCode = await buildCode(source, false);
  const sharedCode = await buildCode(source, true);
  let minifiedInline: string | undefined;
  let minifiedShared: string | undefined;
  try {
    minifiedInline = (await terser.minify(inlineCode, {module: true})).code;
    minifiedShared = (await terser.minify(sharedCode, {module: true})).code;
  } catch (error) {
    const line = (error as {line?: number}).line;
    if (line !== undefined) {
      console.error("Terser input context:", inlineCode.split("\n").slice(Math.max(0, line - 3), line + 2).join("\n"));
    }
    throw error;
  }
  if (minifiedInline === undefined || minifiedShared === undefined) {
    throw new Error("Terser did not return minified output");
  }

  const directory = mkdtempSync(join(tmpdir(), "abap-type-factory-startup-"));
  try {
    const inlinePath = join(directory, "inline.mjs");
    const sharedPath = join(directory, "shared.mjs");
    const runnerPath = join(directory, "runner.mjs");
    writeFileSync(inlinePath, inlineCode);
    writeFileSync(sharedPath, sharedCode);
    const runtimeUrl = pathToFileURL(resolve(process.cwd(), "packages/runtime/build/src/index.js")).href;
    writeFileSync(runnerPath, [
      `import {ABAP, MemoryConsole} from ${JSON.stringify(runtimeUrl)};`,
      "globalThis.abap = new ABAP({console: new MemoryConsole()});",
      "await import(process.argv[2]);",
    ].join("\n"));

    const coldRuns: {inline: number[], shared: number[]} = {inline: [], shared: []};
    const measureOne = (mode: "inline" | "shared") => {
      const modulePath = mode === "inline" ? inlinePath : sharedPath;
      const start = performance.now();
      const child = spawnSync(process.execPath, [runnerPath, pathToFileURL(modulePath).href], {stdio: "ignore"});
      const elapsed = performance.now() - start;
      if (child.error !== undefined) {
        throw child.error;
      } else if (child.status !== 0) {
        throw new Error(`Cold ${mode} module import exited with status ${child.status}`);
      }
      coldRuns[mode].push(elapsed);
    };

    for (let run = 0; run < coldStartRuns; run++) {
      if (run % 2 === 0) {
        measureOne("inline");
        measureOne("shared");
      } else {
        measureOne("shared");
        measureOne("inline");
      }
    }

    return {
      minifier: `Terser ${terserPackage.version}`,
      inlineBytes: Buffer.byteLength(inlineCode, "utf8"),
      sharedBytes: Buffer.byteLength(sharedCode, "utf8"),
      minifiedInlineBytes: Buffer.byteLength(minifiedInline, "utf8"),
      minifiedSharedBytes: Buffer.byteLength(minifiedShared, "utf8"),
      coldStartupRuns: coldStartRuns,
      coldStartup: {
        inline: summarize(coldRuns.inline),
        shared: summarize(coldRuns.shared),
        harness: "fresh Node child process, runtime import, then generated ESM import",
      },
    };
  } finally {
    rmSync(directory, {recursive: true, force: true});
  }
}

async function main() {
  const source = makeFixture();
  const smallSource = `TYPES: BEGIN OF ty_small, value TYPE i, END OF ty_small.\nDATA item TYPE ty_small.`;
  const large = await measureComparison(source);
  const small = await measureComparison(smallSource);
  const largeArtifacts = await measureArtifacts(source);
  const smallArtifacts = await measureArtifacts(smallSource);
  console.log(JSON.stringify({
    fixture: "nested repeated ABAP structures",
    repetitions,
    runs,
    sourceBytes: Buffer.byteLength(source, "utf8"),
    large,
    smallFixture: {
      sourceBytes: Buffer.byteLength(smallSource, "utf8"),
      ...small,
    },
    artifacts: {
      large: largeArtifacts,
      small: smallArtifacts,
    },
    node: process.version,
  }, null, 2));
}

main().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
