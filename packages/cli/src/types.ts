import {ITranspilerOptions} from "@abaplint/transpiler";

export interface ITranspilerConfig {
  input_folder: string | string[];
  /** list of regex, case insensitive, empty gives all files, positive list
   * @uniqueItems true
   */
  input_filter?: string[];
  /** list of regex, case insensitive
   * @uniqueItems true
   */
  exclude_filter?: string[];
  output_folder: string;
  libs?: {
    url?: string,
    folder?: string,
    files?: string | string[],
    /** list of regex, case insensitive
     * @uniqueItems true
     */
    exclude_filter?: string[];
  }[],
  write_unit_tests?: boolean;
  write_source_map?: boolean;

  options: ITranspilerOptions;
}