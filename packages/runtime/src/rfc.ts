export type RFCCallInput = {
  exporting?: {[name: string]: any},
  importing?: {[name: string]: any},
  tables?: {[name: string]: any},
  changing?: {[name: string]: any},
  exceptions?: {[name: string]: number},
};

export interface RFCClient {
  call(name: string, input?: RFCCallInput): Promise<void>;
}