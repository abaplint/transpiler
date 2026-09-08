import * as fs from "fs";
import * as path from "path";
import * as glob from "glob";
import * as Transpiler from "@abaplint/transpiler";
import {ITranspilerConfig} from "./types";
import pLimit from "p-limit";
import * as os from "node:os";
import * as fsPromises from "node:fs/promises";

export class FileOperations {

  public static deleteFolderRecursive(p: string) {
    if (fs.existsSync(p) === false) {
      return;
    }

    fs.rmSync(p, {recursive: true});
  }

  /** posix + absolute make glob return forward slashed absolute paths on all
      platforms, but on Windows in fully resolved UNC form, ie. //?/C:/foo.
      path.relative() does not consider //?/C: and C: the same root, and would
      return the full path instead of a relative one, so strip the prefix */
  private static stripNamespace(filename: string): string {
    if (filename.startsWith("//?/UNC/")) {
      return "//" + filename.substring("//?/UNC/".length);
    } else if (filename.startsWith("//?/")) {
      return filename.substring("//?/".length);
    }
    return filename;
  }

  public static globSync(pattern: string): string[] {
    // backslashes are escape characters in glob patterns, always hand it forward slashes
    const normalized = pattern.split(path.sep).join("/");
    return glob.sync(normalized, {nodir: true, absolute: true, posix: true}).map(f => this.stripNamespace(f));
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
//        const isBinary = filename.includes(".w3mi.data.");
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
    const inputFilters = (config.input_filter ?? []).map(pattern => new RegExp(pattern, "i"));
    const excludeFilters = (config.exclude_filter ?? []).map(pattern => new RegExp(pattern, "i"));
    let skipped = 0;
    let added = 0;

    const folders = Array.isArray(config.input_folder) ? config.input_folder : [config.input_folder];
    const filesToRead: string[] = [];
    for (const folder of folders) {
      for (const filename of this.globSync(folder + "/**")) {
        if (inputFilters.length > 0 && inputFilters.some(a => a.test(filename)) === false) {
          skipped++;
          continue;
        } else if (excludeFilters.length > 0 && excludeFilters.some(a => a.test(filename)) === true) {
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

  public static async writeFiles(files: {path: string, contents: string}[]) {
    const limit = this.setupPLimit();
    const promises = files.map((file) => {
      return limit(async () => {
        await fsPromises.writeFile(file.path, file.contents);
      });
    });
    await Promise.all(promises);
  }

}
