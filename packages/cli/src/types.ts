import {ITranspilerOptions} from "@abaplint/transpiler";

export interface ITranspilerConfig {
  input_folder: string;
  /** list of regex, case insensitive, empty gives all files, positive list */
  input_filter?: string[];
  output_folder: string;
  /** to be deprecated, "lib" */
  lib?: string;
  libs?: {
    url?: string,
    folder?: string,
    files?: string,
  }[],
  write_unit_tests: boolean;
  write_source_map: boolean;
  options: ITranspilerOptions;
}