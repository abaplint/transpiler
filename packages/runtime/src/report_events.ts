import {Context} from "./context";

/** STOP: leaves the selection events of the report, END-OF-SELECTION still runs */
export class ReportStop extends Error {
  public constructor() {
    super("STOP");
  }
}

type EventBlock = () => Promise<unknown>;

/** The event blocks of a report, run in the order of the ABAP runtime when the program is started without its
 * selection screen (SUBMIT, a background job): LOAD-OF-PROGRAM, INITIALIZATION, the AT SELECTION-SCREEN events,
 * START-OF-SELECTION, GET, END-OF-SELECTION. Each block is a function of its own, so RETURN, CHECK and EXIT leave the
 * block. The transpiler writes the calls, see ReportEvents in the transpiler */
export class ReportEvents {
  /** what an event block returns when RETURN or EXIT left it on program level */
  public readonly LEAVE = "LEAVE";
  private stopped = false;
  private left = false;
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  /** the events before START-OF-SELECTION: after a STOP the next event that runs is END-OF-SELECTION */
  public async run(event: string, block: EventBlock): Promise<void> {
    if (this.stopped === true) {
      return;
    }
    this.context.reportEvent = event;
    try {
      await block();
    } catch (e) {
      if (!(e instanceof ReportStop)) {
        throw e;
      }
      this.stopped = true;
    }
  }

  /** START-OF-SELECTION and GET: STOP leaves them and END-OF-SELECTION runs, RETURN and EXIT end the program */
  public async selection(event: string, block: EventBlock): Promise<void> {
    if (this.stopped === true || this.left === true) {
      return;
    }
    this.context.reportEvent = event;
    try {
      if (await block() === this.LEAVE) {
        this.left = true;
      }
    } catch (e) {
      if (!(e instanceof ReportStop)) {
        throw e;
      }
      this.stopped = true;
    }
  }

  public async end(block: EventBlock): Promise<void> {
    if (this.left === true) {
      return;
    }
    this.context.reportEvent = "END-OF-SELECTION";
    try {
      await block();
    } catch (e) {
      if (!(e instanceof ReportStop)) {
        throw e;
      }
    }
  }

  /** TOP-OF-PAGE runs right before the first output to the list, see Statements.topOfPage */
  public topOfPage(block: EventBlock): void {
    this.context.topOfPage = block;
  }

  /** the program ended, a TOP-OF-PAGE that never ran is not run later */
  public done(): void {
    this.context.topOfPage = undefined;
  }
}
