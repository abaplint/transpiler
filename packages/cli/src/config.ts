import * as path from "path";
import {ITranspilerOptions} from "@abaplint/transpiler";
import * as fs from "fs";

export interface ITranspilerConfig {
  input_folder: string;
  /** list of regex, case insensitive, empty gives all files, positive list */
  input_filter?: string[];
  output_folder: string;
  /** to be deprecated */
  lib?: string;
  libs?: {
    url: string,
  }[],
  write_unit_tests: boolean;
  write_source_map: boolean;
  options: ITranspilerOptions;
}

export class TranspilerConfig {

  public static find(filename: string | undefined): ITranspilerConfig {
    if (filename !== undefined) {
      const f = filename;
      if (fs.existsSync(f)) {
        console.log("Using config: " + filename);
        const json = fs.readFileSync(f, "utf8");
        return JSON.parse(json);
      }
    }

    const f = process.cwd() + path.sep + "abap_transpile.json";
    if (fs.existsSync(f)) {
      console.log("Using config: abap_transpile.json");
      const json = fs.readFileSync(f, "utf8");
      return JSON.parse(json);
    }

    console.log("Using default config");
    return this.getDefaultConfig();
  }

  public static getDefaultConfig(): ITranspilerConfig {
    return {
      input_folder: "src",
      input_filter: [],
      output_folder: "output",
      lib: "https://github.com/open-abap/open-abap",
      write_unit_tests: true,
      write_source_map: true,
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