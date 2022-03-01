import {RFCClient} from "../rfc";

export interface ICallFunctionOptions {
  name: string,
  destination: string,
  exporting?: any,
  importing?: any,
  tables?: any,
  changing?: any,
  exceptions?: any,
}

// note: this is only called if DESTNIATION is supplied
export async function callFunction(options: ICallFunctionOptions) {
  // @ts-ignore
  const dest = abap.RFCDestinations[options.destination] as undefined | RFCClient;
  if (dest === undefined) {
    throw new Error(`RFC destination ${options.destination} does not exist`);
  }

  await dest.call(options.name, {
    exporting: options.exporting,
    importing: options.importing,
    tables: options.tables,
    changing: options.changing,
    exceptions: options.exceptions,
  });
}