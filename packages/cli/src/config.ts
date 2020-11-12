import * as path from "path";
import {ITranspilerOptions} from "@abaplint/transpiler";
import * as fs from "fs";

export interface ITranspilerConfig {
  input_folder: string;
  /** list of regex, case insensitive, empty gives all files, positive list */
  input_filter: string[];
  output_folder: string;
  lib: string;
  write_unit_tests: boolean;
  options: ITranspilerOptions;
}

export class TranspilerConfig {

  public static find(): ITranspilerConfig {
    const filename = process.cwd() + path.sep + "abap_transpile.json";
    if (fs.existsSync(filename)) {
      const json = fs.readFileSync(filename, "utf8");
      return JSON.parse(json);
    } else {
      return this.getDefaultConfig();
    }
  }

  public static getDefaultConfig(): ITranspilerConfig {
    return {
      input_folder: "src",
      input_filter: [],
      output_folder: "output",
      lib: "https://github.com/open-abap/open-abap",
      write_unit_tests: true,
      options: {
        ignoreSyntaxCheck: false,
        addFilenames: true,
        addCommonJS: true,
        skipConstants: false,
        unknownTypes: "compileError",
      },
    };
  }
}