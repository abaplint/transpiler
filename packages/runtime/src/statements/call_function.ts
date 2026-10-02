import {Context} from "../context";
import {RFCClient} from "../rfc";
import {throwError} from "../throw_error";
import {Character, FieldSymbol} from "../types";
import {_receiveSetResult} from "./receive";
import {ABAP} from "..";

declare const abap: ABAP;

export interface ICallFunctionOptions {
  name: string,
  updateTask?: boolean,
  destination?: string,
  calling?: (INPUT: any) => any,
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

// called for DESTINATION, CALLING or IN UPDATE TASK
  public async callFunction(options: ICallFunctionOptions) {
    const param = {
      exporting: options.exporting,
      importing: options.importing,
      tables: options.tables,
      changing: options.changing,
      exceptions: options.exceptions,
    };
    options.name = options.name.trimEnd();
    const fm = abap.FunctionModules[options.name];

    if (options.updateTask) {
      if (fm === undefined) {
        throwError("CX_SY_DYN_CALL_ILLEGAL_FUNC");
      }
      const copy = (values: any) => {
        if (values === undefined) {
          return undefined;
        }
        const result: any = {};
        for (const key of Object.keys(values)) {
          const value = values[key];
          result[key] = value instanceof FieldSymbol ? value.getPointer().clone() : value.clone();
        }
        return result;
      };
      // IMPORTING and CHANGING do not apply to update modules.
      const copied = {exporting: copy(options.exporting), tables: copy(options.tables)};
      if (this.context.updateTask) {
        await this.context.updateTask.register(options.name, copied);
      } else {
        await fm(copied);
      }
      return;
    } else if (options.destination) {
      if (options.destination.trim() === "") {
        if (fm === undefined) {
          throwError("CX_SY_DYN_CALL_ILLEGAL_FUNC");
        }
        await fm(param);
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
      if (fm === undefined) {
        throwError("CX_SY_DYN_CALL_ILLEGAL_FUNC");
      }
      await fm(param);

      // save importing + tables + changing + exception for RECEIVE RESULTS
      _receiveSetResult(param);

      // call the callback, async
      options.calling({p_task: new Character(8).set("OPENABAP")});
    } else {
      throw new Error("runtime: callFunction, unexpected input");
    }
  }
}