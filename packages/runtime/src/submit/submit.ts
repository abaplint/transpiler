/** One WITH addition of a SUBMIT, as the caller wrote it. The values are the
 * caller's data objects; the called program converts them into its own
 * PARAMETERS and SELECT-OPTIONS when it declares them */
export type SubmitSelection =
  /** WITH sel = value, or EQ: a parameter's value, or one I EQ row of a select-option */
  {name: string, value: any} |
  /** WITH sel op value [SIGN s], WITH sel BETWEEN low AND high [SIGN s]: one row */
  {name: string, sign: string, option: string, low: any, high?: any} |
  /** WITH sel IN range: every row of a range table */
  {name: string, table: any};

export type SubmitCall = {
  /** the program name, upper case */
  program: string,
  selections: SubmitSelection[],
};

/** Runs a program for SUBMIT ... AND RETURN. The host decides how a name becomes
 * code, eg. the module the transpiler wrote for it. The program runs inside the
 * caller's step: the host must not commit or roll back. An exception the program
 * does not handle ends the caller too, as a runtime error and not as an exception
 * it could catch. */
export interface SubmitHost {
  submit(call: SubmitCall): Promise<void>;
}

/** LEAVE PROGRAM: ends the program; SUBMIT ... AND RETURN continues in the caller.
 * An embedder that runs a program itself catches it */
export class LeaveProgram extends Error {
  public constructor() {
    super("LEAVE PROGRAM");
  }
}

/** A SubmitHost over programs given as functions, eg. the transpiled code of each
 * program wrapped in an async function. Each SUBMIT runs the function again, so the
 * program starts from its declarations every time, as on the system */
export class ProgramRegistry implements SubmitHost {
  private readonly programs: {[name: string]: () => Promise<void>} = {};

  public constructor(programs?: {[name: string]: () => Promise<void>}) {
    for (const name of Object.keys(programs ?? {})) {
      this.add(name, programs![name]);
    }
  }

  public add(name: string, run: () => Promise<void>): void {
    this.programs[name.toUpperCase()] = run;
  }

  public async submit(call: SubmitCall): Promise<void> {
    const run = this.programs[call.program];
    if (run === undefined) {
      // on the system: LOAD_PROGRAM_NOT_FOUND, a runtime error and not an exception
      throw new Error("SUBMIT, program not found: " + call.program);
    }
    await run();
  }
}
