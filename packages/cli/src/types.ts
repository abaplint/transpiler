import {ITranspilerOptions} from "@abaplint/transpiler";

export interface ITranspilerConfig {
  /** @uniqueItems true */
  input_folder: string | string[];
  /** list of regex, case insensitive, empty gives all files, positive list
   * @uniqueItems true
   */
  input_filter?: string[];
  /** list of regex, case insensitive
   * @uniqueItems true
   */
  exclude_filter?: string[];
  /** @minLength 1 */
  output_folder: string;
  /** Skip unchanged generated files and remove tracked obsolete output */
  incremental_output?: boolean;
  /** experimental */
  converter?: {
    /** @uniqueItems true */
    input_folder: string | string[];
    /** @minLength 1 */
    output_folder: string;
  },
  libs?: {
    /** output directory name; defaults to the repository or local folder name
     * @minLength 1
     */
    name?: string,
    url?: string,
    /** relative to the current working directory */
    folder?: string,
    files?: string | string[],
    /** list of regex, case insensitive
     * @uniqueItems true
     */
    exclude_filter?: string[];
    /** module specifier of this library's already transpiled output, e.g.
     * "my-package/output/core"; the library is then read for the type check
     * but not written, and the generated code imports its objects from below
     * this specifier
     * @minLength 1
     */
    import?: string;
  }[],
  /** skip duplicate dependency objects after the first library in libs, with a warning; defaults to false */
  skip_duplicate_dependencies?: boolean;
  write_unit_tests?: boolean;
  write_source_map?: boolean;

  options: ITranspilerOptions;
}
