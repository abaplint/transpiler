import {Context} from "../context";
import {LeaveProgram, SubmitCall, SubmitSelection} from "../submit/submit";
import {Character, String as AString, FieldSymbol, Table} from "../types";
import {ABAP} from "..";

declare const abap: ABAP;

// The rules below were measured on a 7.5x system, SUBMIT ... AND RETURN from a
// report running as a background step:
// - sy-subrc of the caller is the same after the SUBMIT as before it, whatever
//   the called program did, LEAVE PROGRAM included
// - a WITH for a select-option replaces its DEFAULT rows; several WITH for one
//   select-option append rows in the order written
// - WITH p = '' sets a parameter to initial, a parameter not named keeps its DEFAULT
// - without LOWER CASE a character-like parameter is upper-cased, and of a
//   select-option only the first row is, LOW and HIGH; later rows keep their case
// - a value that does not convert ('abc' into TYPE i) is a runtime error that
//   CATCH cx_root around the SUBMIT does not catch, and so is an exception the
//   program does not handle (1 / 0 is COMPUTE_INT_ZERODIVIDE in the caller)
export class SubmitStatement {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public async submit(call: SubmitCall): Promise<void> {
    const host = this.context.submit;
    if (host === undefined) {
      throw new Error("SUBMIT, no host, set RuntimeOptions.submit");
    }
    const subrc = abap.builtin.sy.get().subrc.get();
    this.context.submitted.push(call);
    try {
      await host.submit(call);
    } catch (e) {
      if (e instanceof LeaveProgram) {
        // the program ended, the caller continues
      } else if (e instanceof Error) {
        throw e;
      } else {
        // an exception the program does not handle does not reach the caller, on the
        // system it is a runtime error the caller's CATCH cx_root does not catch
        throw new Error("SUBMIT " + call.program + ", uncaught exception " + (e as any)?.constructor?.name);
      }
    } finally {
      this.context.submitted.pop();
      abap.builtin.sy.get().subrc.set(subrc);
    }
  }

  /** the WITH additions of the SUBMIT that started this program, for one selection */
  private given(program: string, name: string): SubmitSelection[] {
    const call = this.context.submitted[this.context.submitted.length - 1];
    if (call === undefined || call.program !== program) {
      return [];
    }
    return call.selections.filter(s => s.name === name);
  }

  /** a PARAMETERS statement of the program: its value, if the SUBMIT gave one */
  public parameter(program: string, name: string, target: any, lowerCase: boolean): void {
    const given = this.given(program, name);
    if (given.length === 0) {
      return;
    }
    const last = given[given.length - 1];
    if (!("value" in last)) {
      throw new Error("SUBMIT, WITH " + name + " = value expected for a parameter");
    }
    this.convert(name, target, last.value);
    if (lowerCase === false) {
      this.upper(target);
    }
  }

  /** a SELECT-OPTIONS statement of the program: its rows, if the SUBMIT gave some */
  public selectOption(program: string, name: string, target: Table, lowerCase: boolean): void {
    const given = this.given(program, name);
    if (given.length === 0) {
      return;
    }
    target.clear();
    const header = target.getHeader() as any;
    const add = (sign: any, option: any, low: any, high: any) => {
      header.clear();
      header.get().sign.set(sign);
      header.get().option.set(option);
      this.convert(name, header.get().low, low);
      if (high !== undefined) {
        this.convert(name, header.get().high, high);
      }
      if (lowerCase === false && target.getArrayLength() === 0) {
        this.upper(header.get().low);
        this.upper(header.get().high);
      }
      target.append(header);
    };
    for (const s of given) {
      if ("table" in s) {
        let rows = s.table;
        if (rows instanceof FieldSymbol) {
          rows = rows.getPointer();
        }
        for (const row of rows.array()) {
          add(row.get().sign, row.get().option, row.get().low, row.get().high);
        }
      } else if ("value" in s) {
        add("I", "EQ", s.value, undefined);
      } else {
        add(s.sign, s.option, s.low, s.high);
      }
    }
    // the header line keeps the first row, as it keeps a DEFAULT
    header.set(target.array()[0] ?? header);
  }

  private convert(name: string, target: any, value: any) {
    try {
      target.set(value);
    } catch (e: any) {
      // not an exception of the caller: on the system this ends the step
      throw new Error("SUBMIT, WITH " + name + ", value does not convert: " + (e?.message ?? e));
    }
  }

  private upper(target: any) {
    if (target instanceof Character || target instanceof AString) {
      target.set(target.get().toUpperCase());
    }
  }
}
