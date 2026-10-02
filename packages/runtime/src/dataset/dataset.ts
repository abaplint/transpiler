import {FieldSymbol} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

// What DATASET statements need from outside the runtime (a host that hands
// out bytes) and the options the transpiled statements pass in.

export type DatasetMode = "INPUT" | "OUTPUT" | "APPENDING" | "UPDATE";

/** an open file: positional reads and writes, the runtime keeps the position */
export interface DatasetHandle {
  /** up to length bytes from position; fewer at the end of the file */
  read(position: number, length: number): Promise<Uint8Array>;
  write(position: number, bytes: Uint8Array): Promise<void>;
  size(): Promise<number>;
  close(): Promise<void>;
}

export interface DatasetHost {
  /** OUTPUT truncates or creates, APPENDING and UPDATE keep the content;
   *  a failure is returned as the text OPEN DATASET ... MESSAGE receives
   *  and becomes sy-subrc 8 */
  open(name: string, mode: DatasetMode): Promise<DatasetHandle | {message: string}>;
  /** false when there was nothing to delete (sy-subrc 4) */
  delete(name: string): Promise<boolean>;
}

export interface IOpenDatasetOptions {
  mode: DatasetMode;
  binary: boolean;
  encoding?: "DEFAULT" | "UTF-8" | "NON-UNICODE";
  legacy?: boolean;
  message?: ICharacter | FieldSymbol;
  position?: INumeric | FieldSymbol;
  /** additions this runtime does not implement, by name */
  unsupported?: string[];
}

export interface ITransferOptions {
  length?: INumeric | FieldSymbol;
  noEndOfLine?: boolean;
}

export interface IReadDatasetOptions {
  maximumLength?: INumeric | FieldSymbol;
  actualLength?: INumeric | FieldSymbol;
}

export interface IGetDatasetOptions {
  position?: INumeric | FieldSymbol;
  attributes?: any;
}

/** a file OPEN DATASET opened, as the runtime keeps it by name */
export interface OpenDataset {
  handle: DatasetHandle;
  mode: DatasetMode;
  binary: boolean;
  position: number;
}
