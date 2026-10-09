import * as fs from "node:fs";
import * as fsPromises from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import {FileOperations} from "./file_operations";

export interface IOutputArtifact {
  path: string;
  contents: string;
}

export interface IOutputWriteResult {
  created: number;
  updated: number;
  unchanged: number;
  deleted: number;
}

interface IManifest {
  version: 1;
  files: string[];
}

interface IPreparedArtifact extends IOutputArtifact {
  relative: string;
  byteLength: number;
  binary: boolean;
  changed?: boolean;
  existed?: boolean;
}

const MANIFEST = ".abap-transpile-manifest.json";

function concurrency(): number {
  return Math.max(1, Math.min(8, os.cpus().length));
}

async function forEachLimit<T>(items: T[], task: (item: T) => Promise<void>, limit = concurrency()) {
  let next = 0;
  const workers = Array.from({length: Math.min(limit, items.length)}, async () => {
    while (next < items.length) {
      const index = next++;
      await task(items[index]);
    }
  });
  const results = await Promise.allSettled(workers);
  const failed = results.find(result => result.status === "rejected");
  if (failed?.status === "rejected") {
    throw failed.reason;
  }
}

function normalizeRelative(filename: string, label: string): string {
  if (filename.length === 0 || filename.startsWith("/") || filename.includes("\\")
      || /^[a-zA-Z]:/.test(filename)) {
    throw new Error("Invalid " + label + " path: " + filename);
  }
  const segments = filename.split("/");
  if (segments.some(segment => segment.length === 0 || segment === "." || segment === "..")) {
    throw new Error("Invalid " + label + " path: " + filename);
  }
  return segments.join("/");
}

function validateArtifactPaths(artifacts: IOutputArtifact[]): IPreparedArtifact[] {
  const seen = new Set<string>();
  const prepared: IPreparedArtifact[] = [];
  for (const artifact of artifacts) {
    const relative = normalizeRelative(artifact.path.replace(/\\/g, "/"), "output");
    const key = relative.toLowerCase();
    if (key === MANIFEST.toLowerCase() || key.startsWith(MANIFEST.toLowerCase() + "/")) {
      throw new Error("Output path is reserved for the incremental manifest: " + relative);
    }
    if (seen.has(key)) {
      throw new Error("Conflicting output file: " + relative);
    }
    seen.add(key);
    prepared.push({
      ...artifact,
      relative,
      byteLength: Buffer.byteLength(artifact.contents, FileOperations.isBinaryFilename(relative) ? "latin1" : "utf8"),
      binary: FileOperations.isBinaryFilename(relative),
    });
  }
  for (const artifact of prepared) {
    const segments = artifact.relative.split("/");
    for (let index = 1; index < segments.length; index++) {
      if (seen.has(segments.slice(0, index).join("/").toLowerCase())) {
        throw new Error("Conflicting output file path: " + artifact.relative);
      }
    }
  }
  return prepared;
}

function parseManifest(contents: string): IManifest {
  let parsed: unknown;
  try {
    parsed = JSON.parse(contents);
  } catch (error) {
    throw new Error("Invalid incremental output manifest JSON: " + String(error));
  }
  if (typeof parsed !== "object" || parsed === null || !("version" in parsed) || !("files" in parsed)
      || parsed.version !== 1 || !Array.isArray(parsed.files)) {
    throw new Error("Unsupported incremental output manifest");
  }
  const files: string[] = [];
  const seen = new Set<string>();
  for (const entry of parsed.files) {
    if (typeof entry !== "string") {
      throw new Error("Invalid incremental output manifest path");
    }
    const relative = normalizeRelative(entry, "manifest");
    const key = relative.toLowerCase();
    if (key === MANIFEST.toLowerCase() || key.startsWith(MANIFEST.toLowerCase() + "/") || seen.has(key)) {
      throw new Error("Duplicate or reserved path in incremental output manifest: " + relative);
    }
    seen.add(key);
    files.push(relative);
  }
  return {version: 1, files};
}

function serializeManifest(files: Set<string>): string {
  return JSON.stringify({version: 1, files: [...files].sort()}, null, 2) + "\n";
}

function validateCombinedPaths(current: IPreparedArtifact[], previous: string[]) {
  const all = new Map<string, string>();
  for (const path of [...current.map(file => file.relative), ...previous]) {
    const key = path.toLowerCase();
    const existing = all.get(key);
    if (existing !== undefined && existing !== path) {
      throw new Error("Case-insensitive output path collision: " + existing + " and " + path);
    }
    all.set(key, path);
  }
  for (const filename of all.values()) {
    const segments = filename.split("/");
    for (let index = 1; index < segments.length; index++) {
      if (all.has(segments.slice(0, index).join("/").toLowerCase())) {
        throw new Error("Conflicting file and directory paths in incremental output: " + filename);
      }
    }
  }
}

async function lstatOrMissing(filename: string): Promise<fs.Stats | undefined> {
  try {
    return await fsPromises.lstat(filename);
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "ENOENT") {
      return undefined;
    }
    throw error;
  }
}

function isBelowRoot(root: string, filename: string): boolean {
  const relative = path.relative(root, filename);
  return relative !== "" && relative !== ".." && !relative.startsWith(".." + path.sep) && !path.isAbsolute(relative);
}

async function inspectPaths(root: string, relativePaths: string[], rootStats: fs.Stats | undefined, ioConcurrency: number) {
  const metadata = new Map<string, fs.Stats | undefined>();

  const nodes = new Map<string, boolean>();
  for (const filename of relativePaths) {
    const segments = filename.split("/");
    for (let index = 1; index <= segments.length; index++) {
      const relative = segments.slice(0, index).join("/");
      const isDirectory = index < segments.length;
      nodes.set(relative, (nodes.get(relative) ?? false) || isDirectory);
    }
  }

  if (rootStats !== undefined) {
    const byDepth = new Map<number, string[]>();
    for (const relative of nodes.keys()) {
      const depth = relative.split("/").length;
      const group = byDepth.get(depth) ?? [];
      group.push(relative);
      byDepth.set(depth, group);
    }
    for (const depth of [...byDepth.keys()].sort((a, b) => a - b)) {
      const candidates = byDepth.get(depth)!.filter(relative => {
        const parent = path.posix.dirname(relative);
        return depth === 1 || metadata.get(parent)?.isDirectory() === true;
      });
      await forEachLimit(candidates, async relative => {
        const absolute = path.resolve(root, ...relative.split("/"));
        const stat = await lstatOrMissing(absolute);
        if (stat?.isSymbolicLink()) {
          throw new Error("Output path must not traverse a symlink: " + relative);
        }
        if (stat !== undefined && nodes.get(relative) === true && !stat.isDirectory()) {
          throw new Error("Output path parent is not a directory: " + relative);
        }
        if (stat !== undefined && nodes.get(relative) === false && !stat.isFile()) {
          throw new Error("Generated output path is not a regular file: " + relative);
        }
        metadata.set(relative, stat);
      }, ioConcurrency);
    }
  } else {
    for (const relative of nodes.keys()) {
      metadata.set(relative, undefined);
    }
  }
  return metadata;
}

async function atomicWriteManifest(root: string, contents: string, current?: string): Promise<void> {
  if (current === contents) {
    return;
  }
  await fsPromises.mkdir(root, {recursive: true});
  const temporary = path.join(root, MANIFEST + ".tmp-" + process.pid + "-" + Math.random().toString(36).slice(2));
  try {
    await fsPromises.writeFile(temporary, contents, {flag: "wx", encoding: "utf8"});
    await fsPromises.rename(temporary, path.join(root, MANIFEST));
  } catch (error) {
    try {
      await fsPromises.unlink(temporary);
    } catch {
      // The temporary file can be absent when creation failed.
    }
    throw error;
  }
}

export async function writeOutput(rootFolder: string, artifacts: IOutputArtifact[], incremental: boolean,
                                  ioConcurrency = concurrency()): Promise<IOutputWriteResult> {
  const root = path.resolve(rootFolder);
  const prepared = validateArtifactPaths(artifacts);
  const result: IOutputWriteResult = {
    created: 0, updated: 0, unchanged: 0, deleted: 0,
  };

  if (!incremental) {
    const files = prepared.map(file => ({path: path.resolve(root, file.relative), contents: file.contents}));
    await FileOperations.writeFiles(files);
    result.updated = files.length;
    return result;
  }

  const manifestPath = path.join(root, MANIFEST);
  const rootStats = await lstatOrMissing(root);
  if (rootStats?.isSymbolicLink()) {
    throw new Error("Output folder must not be a symlink: " + root);
  }
  if (rootStats !== undefined && !rootStats.isDirectory()) {
    throw new Error("Output folder is not a directory: " + root);
  }
  let manifestContents: string | undefined;
  let previous: string[] = [];
  if (rootStats !== undefined) {
    const manifestStat = await lstatOrMissing(manifestPath);
    if (manifestStat?.isSymbolicLink() || (manifestStat !== undefined && !manifestStat.isFile())) {
      throw new Error("Incremental output manifest must be a regular file");
    }
    if (manifestStat !== undefined) {
      const contents = await fsPromises.readFile(manifestPath, "utf8");
      manifestContents = contents;
      previous = parseManifest(contents).files;
    }
  }

  validateCombinedPaths(prepared, previous);
  const previousPaths = new Set(previous);
  const current = new Set(prepared.map(file => file.relative));
  const stale = previous.filter(filename => !current.has(filename));
  const additions = prepared.some(file => !previousPaths.has(file.relative));
  const pathsToInspect = [...new Set([...prepared.map(file => file.relative), ...stale])];
  const metadata = await inspectPaths(root, pathsToInspect, rootStats, ioConcurrency);

  await forEachLimit(prepared, async file => {
    const stat = metadata.get(file.relative);
    file.existed = stat !== undefined;
    if (stat === undefined || stat.size !== file.byteLength) {
      file.changed = true;
      return;
    }
    let existing: Buffer;
    try {
      existing = await fsPromises.readFile(path.resolve(root, ...file.relative.split("/")));
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code === "ENOENT") {
        file.existed = false;
        file.changed = true;
        return;
      }
      throw error;
    }
    const generated = Buffer.from(file.contents, file.binary ? "latin1" : "utf8");
    file.changed = !existing.equals(generated);
  }, ioConcurrency);

  if (additions) {
    const union = serializeManifest(new Set([...previous, ...current]));
    await atomicWriteManifest(root, union, manifestContents);
    manifestContents = union;
  }

  const toWrite = prepared.filter(file => file.changed);
  const directories = new Map<string, number>();
  for (const file of toWrite) {
    const segments = file.relative.split("/");
    for (let index = 1; index < segments.length; index++) {
      const relative = segments.slice(0, index).join("/");
      if (metadata.get(relative) === undefined) {
        directories.set(relative, index);
      }
    }
  }
  const directoryDepths = [...new Set(directories.values())].sort((a, b) => a - b);
  for (const depth of directoryDepths) {
    const atDepth = [...directories.entries()].filter(([, candidateDepth]) => candidateDepth === depth)
      .map(([relative]) => path.resolve(root, ...relative.split("/")));
    await forEachLimit(atDepth, async directory => {
      await fsPromises.mkdir(directory);
    }, ioConcurrency);
  }
  await forEachLimit(toWrite, async file => {
    await fsPromises.writeFile(path.resolve(root, ...file.relative.split("/")), file.contents,
      file.binary ? {encoding: "latin1"} : undefined);
  }, ioConcurrency);

  const staleFiles = stale.map(filename => path.resolve(root, ...filename.split("/")));
  const removedDirectories = new Set<string>();
  for (const filename of staleFiles) {
    try {
      await fsPromises.unlink(filename);
      result.deleted++;
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== "ENOENT") {
        throw error;
      }
    }
    let directory = path.dirname(filename);
    while (isBelowRoot(root, directory)) {
      removedDirectories.add(directory);
      directory = path.dirname(directory);
    }
  }
  await forEachLimit([...removedDirectories].sort((a, b) => b.length - a.length), async directory => {
    try {
      await fsPromises.rmdir(directory);
    } catch (error) {
      const code = (error as NodeJS.ErrnoException).code;
      if (code !== "ENOENT" && code !== "ENOTEMPTY" && code !== "EEXIST") {
        throw error;
      }
    }
  });

  const finalManifest = serializeManifest(current);
  await atomicWriteManifest(root, finalManifest, manifestContents);
  result.created = prepared.filter(file => file.changed && !file.existed).length;
  result.updated = prepared.filter(file => file.changed && file.existed).length;
  result.unchanged = prepared.filter(file => !file.changed).length;
  return result;
}
