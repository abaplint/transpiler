import * as fs from "fs";
import * as path from "path";
import * as glob from "glob";
import * as Transpiler from "@abaplint/transpiler";
import {ITranspilerConfig} from "./types";
import * as pLimit from "p-limit";
import * as os from "node:os";
import * as fsPromises from "node:fs/promises";

export class FileOperations {

  public static deleteFolderRecursive(p: string) {
    if (fs.existsSync(p) === false) {
      return;
    }

    fs.rmSync(p, {recursive: true});
  }

  private static setupPLimit() {
    let concurrency = os.cpus().length;
    if (concurrency > 8) {
      concurrency = 8;
    } else if (concurrency < 1) {
      concurrency = 1;
    }
    return pLimit(concurrency);
  }

  public static async readAllFiles(filesToRead: string[], outputFolder: string) {
    const limit = this.setupPLimit();
    const promises = filesToRead.map((filename) => {
      return limit(async () => {
        return {
          filename: path.basename(filename),
          relative: path.relative(outputFolder, path.dirname(filename)),
          contents: await fsPromises.readFile(filename, "utf8"),
        };
      });
    });
    return Promise.all(promises);
  }

  public static async loadFiles(config: ITranspilerConfig): Promise<Transpiler.IFile[]> {
    const filter = (config.input_filter ?? []).map(pattern => new RegExp(pattern, "i"));
    let skipped = 0;
    let added = 0;

    const folders = Array.isArray(config.input_folder) ? config.input_folder : [config.input_folder];
    const filesToRead: string[] = [];
    for (const folder of folders) {
      for (const filename of glob.sync(folder + "/**", {nosort: true, nodir: true})) {
        if (filter.length > 0 && filter.some(a => a.test(filename)) === false) {
          skipped++;
          continue;
        }
        filesToRead.push(filename);
        added++;
      }
    }

    const files = await this.readAllFiles(filesToRead, config.output_folder);

    console.log(added + " files added from source");
    console.log(skipped + " files skipped in source");
    return files;
  }

}