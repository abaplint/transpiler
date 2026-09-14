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

  // abapGit stores the content of a Web Repository or MIME object beside its
  // XML, in a file whose name says .w3mi.data. or .smim.data. That content is
  // bytes, and reading it as UTF-8 destroys it: every byte above 0x7F fails to
  // decode and comes back as the replacement character, so a PNG arrives
  // starting ef bf bd rather than 89 50 4e 47 and is served, with a plausible
  // size and the right content type, as a broken image.
  //
  // latin1 rather than a Buffer because the whole pipeline carries a file as a
  // string; latin1 maps every byte to the code point of the same value, so a
  // read and a write of it are exact. Node's "binary" is the same encoding
  // under its old name, so there is nothing more faithful to switch to: the
  // only stronger answer is to carry a Buffer end to end and never make it a
  // string at all, which is a change to every file the transpiler touches.
  // Measured on an 11770 byte PNG: latin1 and binary both return it
  // unchanged, utf8 returns 20175 bytes and no longer a PNG.
  public static isBinaryFilename(filename: string): boolean {
    return /\.(w3mi|smim)\.data\./i.test(filename);
  }

  public static async readAllFiles(filesToRead: string[], outputFolder: string) {
    const limit = this.setupPLimit();
    const promises = filesToRead.map((filename) => {
      return limit(async () => {
        return {
          filename: path.basename(filename),
          relative: path.relative(outputFolder, path.dirname(filename)),
          contents: await fsPromises.readFile(filename, this.isBinaryFilename(filename) ? "latin1" : "utf8"),
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
        // read as latin1, written as latin1: anything else re-encodes the
        // bytes on the way out and undoes the careful read
        await fsPromises.writeFile(file.path, file.contents,
                                   this.isBinaryFilename(file.path) ? {encoding: "latin1"} : undefined);
      });
    });
    await Promise.all(promises);
  }

}
