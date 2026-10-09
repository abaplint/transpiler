import * as path from "path";
import {pathToFileURL} from "url";
import {IOutputFile, importPath} from "@abaplint/transpiler";
import {ITranspilerConfig} from "./types";
import {ISourceFile} from "./libraries";
import {IOutputArtifact} from "./output_writer";

export function collectObjectFiles(outputFiles: IOutputFile[], config: ITranspilerConfig, files: ISourceFile[]) {
  const root = path.resolve(config.output_folder);
  const filesToWrite: IOutputArtifact[] = [];
  const sources = new Map(files.map(file => [file.filename.toLowerCase(), file]));
  const add = (filename: string, contents: string) => {
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
        if (file.sourceMapPath !== undefined) {
          sourcePaths[filename] = file.sourceMapPath;
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
