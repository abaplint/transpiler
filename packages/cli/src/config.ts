import * as path from "path";
import * as fs from "fs";
import {ITranspilerConfig} from "./types";
import {UnknownTypesEnum} from "@abaplint/transpiler";

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
      libs: [
        {"url": "https://github.com/open-abap/open-abap-core"},
      ],
      write_unit_tests: true,
      write_source_map: true,
      options: {
        ignoreSyntaxCheck: false,
        addFilenames: true,
        addCommonJS: true,
        skipConstants: false,
        unknownTypes: UnknownTypesEnum.compileError,
      },
    };
  }
}