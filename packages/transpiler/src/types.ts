import {Chunk} from "./chunk";
import * as abaplint from "@abaplint/core";
import {TestMethodList} from "./unit_test";

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
  databaseSetup: string;
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
  unknownTypes?: "compileError" | "runtimeError";
  skip?: TestMethodList;
  only?: TestMethodList;
  /** extra setup script to be executed during initialization */
  extraSetup?: string;
}