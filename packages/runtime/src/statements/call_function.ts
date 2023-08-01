import {Context} from "../context";
import {RFCClient} from "../rfc";

export interface ICallFunctionOptions {
  name: string,
  destination?: string,
  calling?: () => any,
  exporting?: any,
  importing?: any,
  tables?: any,
  changing?: any,
  exceptions?: any,
}

export class CallFunction {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

// note: this is only called if DESTINIATION is supplied
  public async callFunction(options: ICallFunctionOptions) {
    const param = {
      exporting: options.exporting,
      importing: options.importing,
      tables: options.tables,
      changing: options.changing,
      exceptions: options.exceptions,
    };
    options.name = options.name.trimEnd();

    if (options.destination) {
      if (options.destination.trim() === "") {
        // @ts-ignore
        await abap.FunctionModules[options.name](param);
        return;
      }

      const dest = this.context.RFCDestinations[options.destination] as undefined | RFCClient;
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
    } else if (options.calling) {
      // @ts-ignore
      await abap.FunctionModules[options.name](param);

      // save importing + tables + changing + exception for RECEIVE RESULTS
      // todo

      throw new Error("runtime: callFunction, todo calling");
    } else {
      throw new Error("runtime: callFunction, unexpected input");
    }
  }
}