import {FieldSymbol, Structure, Table} from "./types/index.js";
import {ICharacter} from "./types/_character.js";
import {INumeric} from "./types/_numeric.js";

type TypeType = INumeric | Table | ICharacter | Structure | FieldSymbol;

export type RFCCallSignature = {
  exporting?: {[name: string]: TypeType},
  importing?: {[name: string]: TypeType},
  tables?: {[name: string]: TypeType},
  changing?: {[name: string]: TypeType},
  exceptions?: {[name: string]: number},
};

export interface RFCClient {
  call(name: string, signature: RFCCallSignature): Promise<void>;
}