import {Chunk} from "./chunk";
import * as abaplint from "@abaplint/core";
import {TestMethodList} from "./unit_test";
import {DatabaseSetupResult} from "./db/database_setup_result";

export interface IFile {
  filename: string,
  // from output folder to original source folder
  relative?: string,
  contents: string,
}

export interface IObjectIdentifier {
  name: string,
  type: string,
}

export interface IOutput {
  objects: IOutputFile[];
  reg: abaplint.IRegistry;
  unitTestScript: string;
  unitTestScriptOpen: string;
  initializationScript: string;
  initializationScript2: string;
  databaseSetup: DatabaseSetupResult;
}

export interface IRequire {
  name: string | undefined,
  filename: string,
}

export interface IProgress {
  set(total: number, text: string): void;
  tick(text: string): Promise<void>;
}

/** one javascript output file for each object */
export interface IOutputFile {
  object: IObjectIdentifier;
  filename: string,
  chunk: Chunk,
  requires: readonly IRequire[];
  exports: readonly string[];
}

export enum UnknownTypesEnum {
  compileError = "compileError",
  runtimeError = "runtimeError",
}

export interface ITranspilerOptions {
  /** ignore syntax check, used for internal testing */
  ignoreSyntaxCheck?: boolean;
  /** adds common js modules */
  addCommonJS?: boolean;
  /** adds filenames as comments in the output js */
  addFilenames?: boolean;
  /** skip outputing constants, used for internal testing */
  skipConstants?: boolean;
  /** sets behavior for unknown types, either fail at compile- or run-time */
  unknownTypes?: UnknownTypesEnum;
  /** list of unit tests to skip */
  skip?: TestMethodList;
  /** extra setup script to be executed during initialization */
  extraSetup?: string;
  /** list of keywords to rename, if not supplied default will be used */
  keywords?: string[];
  /** populate tables, all tables are populated if undefined and they exist */
  populateTables?: {
    /** insert into REPOSRC, skips if equals false */
    reposrc?: boolean;
    /** insert into SEOSUBCO, skips if equals false */
    seosubco?: boolean;
    /** insert into SEOSUBCODF, skips if equals false */
    seosubcodf?: boolean;
    /** insert into SEOSUBCOTX, skips if equals false */
    seosubcotx?: boolean;
  };
  /** ignore source map */
  ignoreSourceMap?: boolean;
  /** import programs */
  importProg?: boolean;
  /** skips version check, not recommended */
  skipVersionCheck?: boolean;
}