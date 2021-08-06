import * as fs from "fs";
import * as path from "path";
import * as glob from "glob";
import {ITranspilerConfig} from "./config";
import * as Transpiler from "@abaplint/transpiler";

export class FileOperations {

  public static deleteFolderRecursive(p: string) {
    if (fs.existsSync(p) === false) {
      return;
    }

    const files = fs.readdirSync(p);
    for (const file of files) {
      const curPath = p + path.sep + file;
      if (fs.lstatSync(curPath).isDirectory()) {
        this.deleteFolderRecursive(curPath);
      } else {
        fs.unlinkSync(curPath);
      }
    }
    fs.rmdirSync(p);
  }

  public static loadFiles(config: ITranspilerConfig): Transpiler.IFile[] {
    const files: Transpiler.IFile[] = [];
    const filter = (config.input_filter ?? []).map(pattern => new RegExp(pattern, "i"));
    let skipped = 0;

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
      console.log("Add:\t" + filename);
    }
    console.log(skipped + " files skipped");
    return files;
  }

}