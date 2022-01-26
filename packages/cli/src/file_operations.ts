import * as fs from "fs";
import * as path from "path";
import * as glob from "glob";
import * as Transpiler from "@abaplint/transpiler";
import {ITranspilerConfig} from "./types";

export class FileOperations {

  public static deleteFolderRecursive(p: string) {
    if (fs.existsSync(p) === false) {
      return;
    }

    fs.rmSync(p, {recursive: true});
  }

  public static loadFiles(config: ITranspilerConfig): Transpiler.IFile[] {
    const files: Transpiler.IFile[] = [];
    const filter = (config.input_filter ?? []).map(pattern => new RegExp(pattern, "i"));
    let skipped = 0;
    let added = 0;

    for (const filename of glob.sync(config.input_folder + "/**", {nosort: true, nodir: true})) {
      if (filter.length > 0 && filter.some(a => a.test(filename)) === false) {
        skipped++;
        continue;
      }
      files.push({
        filename: path.basename(filename),
        relative: path.relative(config.output_folder, path.dirname(filename)),
        contents: fs.readFileSync(filename, "utf8"),
      });
      added++;
    }
    console.log(added + " files added");
    console.log(skipped + " files skipped");
    return files;
  }

}