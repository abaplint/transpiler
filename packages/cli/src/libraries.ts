import * as fs from "fs";
import * as path from "path";
import * as childProcess from "child_process";
import * as os from "os";
import * as abaplint from "@abaplint/core";
import {IFile} from "@abaplint/transpiler";
import {ITranspilerConfig} from "./types";
import {FileOperations} from "./file_operations";
import {buildGitCloneArguments} from "./git_clone";
import {resolveLibFolder} from "./lib_folder";

type Library = NonNullable<ITranspilerConfig["libs"]>[number];

export interface LoadedLibrary {
  name: string;
  files: ISourceFile[];
}

export interface ISourceFile extends IFile {
  sourceMapPath?: string;
}

function logicalLibrarySource(library: string, directory: string, filename: string): string {
  const relative = path.relative(directory, filename).split(path.sep)
    .map(segment => encodeURIComponent(segment)).join("/");
  return "abaplint://" + encodeURIComponent(library) + "/" + relative;
}

export function libraryNames(libs: readonly Library[]): string[] {
  const used = new Set(["project"]);
  return libs.map(lib => {
    const source = (lib.url || lib.folder || "").replace(/\\/g, "/").replace(/\/+$/, "");
    const name = lib.name ?? source.substring(Math.max(source.lastIndexOf("/"), source.lastIndexOf(":")) + 1)
      .replace(/\.git$/i, "");
    if (typeof name !== "string" || name.length === 0 || name === "." || name === ".."
        || /[<>:"/\\|?*\u0000-\u001f]/.test(name) || /[. ]$/.test(name)
        || /^(con|prn|aux|nul|com[1-9¹²³]|lpt[1-9¹²³])(\.|$)/i.test(name)) {
      throw new Error("Invalid library name: " + JSON.stringify(name) + ". Set libs[].name to a valid folder name.");
    }
    if (used.has(name.toLowerCase())) {
      throw new Error("Duplicate or reserved library name: " + name + ". Set a different libs[].name.");
    }
    used.add(name.toLowerCase());
    return name;
  });
}

export async function loadLibraries(config: ITranspilerConfig): Promise<LoadedLibrary[]> {
  const libs = config.libs || [];
  const names = libraryNames(libs);
  const result: LoadedLibrary[] = [];
  for (const [index, lib] of libs.entries()) {
    const libraryName = names[index];
    let dir: string;
    let cleanupFolder = false;
    const folder = resolveLibFolder(lib.folder, process.cwd());
    if (folder !== undefined && fs.existsSync(folder)) {
      console.log("From folder: " + folder);
      dir = folder;
    } else {
      if (!lib.url) {
        throw new Error(folder === undefined
          ? "Library must define a non-empty url or an existing folder"
          : "Library folder not found: " + folder);
      }
      console.log("Clone: " + lib.url);
      dir = fs.mkdtempSync(path.join(os.tmpdir(), "abap_transpile-"));
      cleanupFolder = true;
    }
    try {
      if (cleanupFolder) {
        childProcess.execFileSync("git", buildGitCloneArguments(lib.url!), {cwd: dir, stdio: "inherit"});
      }
      const patterns = typeof lib.files === "string" && lib.files !== "" ? [lib.files]
        : Array.isArray(lib.files) ? lib.files : ["/src/**"];
      const excludeFilters = (lib.exclude_filter ?? []).map(pattern => new RegExp(pattern, "i"));
      const filesToRead = new Set<string>();
      for (const pattern of patterns) {
        for (const filename of FileOperations.globSync(dir + pattern)) {
          if (filename.endsWith(".clas.testclasses.abap") || excludeFilters.some(filter => filter.test(filename))) {
            continue;
          }
          filesToRead.add(filename);
        }
      }
      const filenames = [...filesToRead];
      const files: ISourceFile[] = await FileOperations.readAllFiles(filenames, config.output_folder);
      if (cleanupFolder) {
        for (const [index, file] of files.entries()) {
          file.sourceMapPath = logicalLibrarySource(libraryName, dir, filenames[index]);
          delete file.relative;
        }
      }
      result.push({name: libraryName, files});
      console.log("\t" + files.length + " files added from lib");
    } finally {
      if (cleanupFolder) {
        FileOperations.deleteFolderRecursive(dir);
      }
    }
  }
  return result;
}

export function libraryRegistry(files: IFile[], libraries: LoadedLibrary[], skipDuplicateDependencies = false) {
  const reg = new abaplint.Registry();
  const folders = new Map<string, string>();
  const objects = new Map<string, string>();
  const sources = new Map<string, ISourceFile>();
  // Libraries first: addFile() then replaces a dependency object with the project's object.
  for (const lib of libraries) {
    const skipped = new Set<string>();
    for (const file of lib.files) {
      const memory = new abaplint.MemoryFile(file.filename, file.contents);
      const type = memory.getObjectType()?.toUpperCase();
      if (type === undefined || file.filename.split(".").length <= 2) {
        continue;
      }
      const key = type + ":" + memory.getObjectName().toUpperCase();
      const owner = objects.get(key);
      // abapGit repeats package.devc.xml across repositories; the registry keeps the last copy.
      if (key !== "DEVC:PACKAGE" && owner !== undefined && owner !== lib.name) {
        if (skipDuplicateDependencies === true) {
          if (!skipped.has(key)) {
            console.warn("Skipping duplicate dependency object " + key + " in " + lib.name + "; using " + owner);
            skipped.add(key);
          }
          // Skip every file of the losing object, including locals, metadata, and binary assets.
          continue;
        }
        throw new Error("Ambiguous dependency object " + key + " in " + owner + " and " + lib.name);
      }
      objects.set(key, lib.name);
      reg.addDependency(memory);
      folders.set(file.filename.toLowerCase(), lib.name);
      sources.set(file.filename.toLowerCase(), file);
    }
  }
  for (const file of files) {
    reg.addFile(new abaplint.MemoryFile(file.filename, file.contents));
    folders.set(file.filename.toLowerCase(), "project");
    sources.set(file.filename.toLowerCase(), file);
  }
  return {reg, folders, sources: [...sources.values()]};
}
