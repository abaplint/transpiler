import * as path from "path";
import {pathToFileURL} from "url";
import {IFile, IOutputFile, importPath} from "@abaplint/transpiler";
import {FileOperations} from "./file_operations";
import {ITranspilerConfig} from "./types";

export function collectObjectFiles(outputFiles: IOutputFile[], config: ITranspilerConfig, files: IFile[],
                                  sourceMapPaths?: Map<string, string>) {
  const root = path.resolve(config.output_folder);
  const filesToWrite: {path: string, contents: string}[] = [];
  const destinations = new Set<string>();
  const sources = new Map(files.map(file => [file.filename.toLowerCase(), file]));
  const add = (filename: string, contents: string) => {
    const target = path.resolve(root, filename);
    const relative = path.relative(root, target);
    if (relative === "" || relative.startsWith(".." + path.sep) || relative === ".." || path.isAbsolute(relative)) {
      throw new Error("Output file escapes output_folder: " + filename);
    }
    const key = target.toLowerCase();
    if (destinations.has(key)) {
      throw new Error("Conflicting output file: " + filename);
    }
    destinations.add(key);
    filesToWrite.push({path: filename.replace(/\\/g, "/"), contents});
  };

  for (const output of outputFiles) {
    const type = output.object.type.toUpperCase();
    let contents = output.chunk.getCode();
    let generatedLineOffset = 0;
    if (type === "PROG") {
      contents = 'if (!globalThis.abap) await import("' + importPath(output.filename, "_init.mjs") + '");\n' + contents;
      generatedLineOffset = 1;
    }
    if (config.write_source_map === true && (type === "PROG" || type === "FUGR" || type === "CLAS")) {
      const name = output.filename + ".map";
      contents += "\n//# sourceMappingURL=" + encodeURIComponent(path.basename(name));
      const sourcePaths: {[filename: string]: string} = {};
      const sourceContents: {[filename: string]: string} = {};
      const mapDirectory = path.dirname(path.join(root, name));
      for (const filename of new Set(output.chunk.mappings.map(mapping => mapping.source))) {
        const file = sources.get(filename.toLowerCase());
        if (file === undefined) {
          continue;
        }
        const logicalSource = sourceMapPaths?.get(filename.toLowerCase());
        if (logicalSource !== undefined) {
          sourcePaths[filename] = logicalSource;
        } else if (file.relative !== undefined) {
          const source = path.resolve(root, file.relative, file.filename);
          const relative = path.relative(mapDirectory, source);
          // Sources on another Windows drive cannot be expressed as relative paths.
          sourcePaths[filename] = path.isAbsolute(relative) ? pathToFileURL(source).href
            : relative.split(path.sep).map(segment => encodeURIComponent(segment)).join("/");
        }
        // Embedding source text keeps maps useful after temporary checkouts are removed.
        sourceContents[filename] = file.contents;
      }
      add(name, output.chunk.getMap(path.basename(output.filename), {generatedLineOffset, sourcePaths, sourceContents}));
    }
    add(output.filename, contents);
  }
  return filesToWrite;
}

export async function writeObjects(outputFiles: IOutputFile[], config: ITranspilerConfig, files: IFile[],
                                    sourceMapPaths?: Map<string, string>) {
  const filesToWrite = collectObjectFiles(outputFiles, config, files, sourceMapPaths);
  const root = path.resolve(config.output_folder);
  await FileOperations.writeFiles(filesToWrite.map(file => ({...file, path: path.resolve(root, file.path)})));
}
