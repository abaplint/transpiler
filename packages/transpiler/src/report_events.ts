import * as abaplint from "@abaplint/core";
import {Chunk} from "./chunk";
import {filesWithProgramIncludes} from "./program_includes";

/** The event blocks of a report that the program runs when it is started without its selection screen (SUBMIT, a
 * background job), in the order of the ABAP runtime. Blocks only a user's action starts (AT LINE-SELECTION, AT
 * USER-COMMAND, F4 help, ...) do not run. Other blocks of the same event are appended in source order */
const RUN_ORDER = [
  "LOAD-OF-PROGRAM",
  "INITIALIZATION",
  "AT SELECTION-SCREEN OUTPUT",
  "AT SELECTION-SCREEN ON",
  "AT SELECTION-SCREEN",
  "TOP-OF-PAGE",
  "START-OF-SELECTION",
  "GET",
  "END-OF-SELECTION",
] as const;

export type ReportEvent = typeof RUN_ORDER[number];

/** Collects the code of a report's event blocks while the program, and the INCLUDEs expanded into it, are
 * traversed; then writes them as functions run by the runtime's ReportEvents, see runtime report_events.ts */
export class ReportEvents {
  /** code before the first event statement belongs to START-OF-SELECTION */
  private current: ReportEvent | null = "START-OF-SELECTION";
  private readonly blocks = new Map<ReportEvent, Chunk>();
  private readonly selectionScreen: boolean;

  /** selectionScreen: the program has one, else the AT SELECTION-SCREEN events do not run */
  public constructor(selectionScreen: boolean) {
    this.selectionScreen = selectionScreen;
  }

  /** The event block the statement starts: undefined when it starts none, null for a block that does not run when
   * the program is started without a user, see RUN_ORDER */
  public static eventOf(node: abaplint.Nodes.StatementNode): ReportEvent | null | undefined {
    const s = node.get();
    // the words of the statement itself, not the names in it, eg. "AT SELECTION-SCREEN ON VALUE-REQUEST FOR"
    const words = " " + node.getChildren().filter(c => c instanceof abaplint.Nodes.TokenNode)
      .map(c => c.getFirstToken().getStr().toUpperCase()).join(" ").replace(/ - /g, "-") + " ";
    const has = (...list: string[]) => list.some(w => words.includes(" " + w + " "));
    if (s instanceof abaplint.Statements.LoadOfProgram) {
      return "LOAD-OF-PROGRAM";
    } else if (s instanceof abaplint.Statements.Initialization) {
      return "INITIALIZATION";
    } else if (s instanceof abaplint.Statements.AtSelectionScreen) {
      if (has("VALUE-REQUEST", "HELP-REQUEST", "EXIT-COMMAND")) {
        return null;
      } else if (has("OUTPUT")) {
        return "AT SELECTION-SCREEN OUTPUT";
      }
      return has("ON") ? "AT SELECTION-SCREEN ON" : "AT SELECTION-SCREEN";
    } else if (s instanceof abaplint.Statements.TopOfPage) {
      return has("DURING") ? null : "TOP-OF-PAGE";
    } else if (s instanceof abaplint.Statements.StartOfSelection) {
      return "START-OF-SELECTION";
    } else if (s instanceof abaplint.Statements.Get) {
      return "GET";
    } else if (s instanceof abaplint.Statements.EndOfSelection) {
      return "END-OF-SELECTION";
    } else if (s instanceof abaplint.Statements.AtLineSelection
        || s instanceof abaplint.Statements.AtUserCommand
        || s instanceof abaplint.Statements.AtPF
        || s instanceof abaplint.Statements.EndOfPage) {
      return null;
    }
    return undefined;
  }

  /** a program with event statements or STOP runs its code as event blocks; a program with START-OF-SELECTION
   * only runs its code as it is written */
  public static needed(obj: abaplint.ABAPObject, reg: abaplint.IRegistry): boolean {
    if (!(obj instanceof abaplint.Objects.Program) || obj.isInclude() === true) {
      return false;
    }
    return filesWithProgramIncludes(obj, reg).some(f => f.getStatements().some(s => {
      const event = ReportEvents.eventOf(s);
      return (event !== undefined && event !== "START-OF-SELECTION") || s.get() instanceof abaplint.Statements.Stop;
    }));
  }

  /** PARAMETERS and SELECT-OPTIONS make the selection screen of a program */
  public static hasSelectionScreen(obj: abaplint.ABAPObject, reg: abaplint.IRegistry): boolean {
    return obj instanceof abaplint.Objects.Program
      && filesWithProgramIncludes(obj, reg).some(f => f.getStatements().some(s =>
        s.get() instanceof abaplint.Statements.Parameter || s.get() instanceof abaplint.Statements.SelectOption));
  }

  /** the program has a TOP-OF-PAGE that runs, its list output first runs the heading */
  public static hasTopOfPage(obj: abaplint.ABAPObject, reg: abaplint.IRegistry): boolean {
    return obj instanceof abaplint.Objects.Program
      && filesWithProgramIncludes(obj, reg).some(f => f.getStatements().some(s => ReportEvents.eventOf(s) === "TOP-OF-PAGE"));
  }

  /** the following code belongs to this event, null: to a block that does not run */
  public switchTo(event: ReportEvent | null): void {
    this.current = event?.startsWith("AT SELECTION-SCREEN") === true && this.selectionScreen === false ? null : event;
  }

  /** false when the current block does not run, its code is not needed */
  public isRunning(): boolean {
    return this.current !== null;
  }

  public add(code: Chunk): void {
    if (this.current === null) {
      return;
    }
    let block = this.blocks.get(this.current);
    if (block === undefined) {
      block = new Chunk();
      this.blocks.set(this.current, block);
    }
    block.appendChunk(code);
  }

  /** the calls of the event blocks, in the order of the runtime, nothing when no block has code */
  public emit(): Chunk {
    const ret = new Chunk();
    const blocks = RUN_ORDER.filter(e => this.blocks.get(e)?.getCode().trim());
    if (blocks.length === 0) {
      return ret;
    }
    ret.appendString("const $events = abap.statements.reportEvents();\n");
    for (const event of blocks) {
      if (event === "TOP-OF-PAGE") {
        ret.appendString("$events.topOfPage(async () => {\n");
      } else if (event === "START-OF-SELECTION" || event === "GET") {
        ret.appendString(`await $events.selection("${event}", async () => {\n`);
      } else if (event === "END-OF-SELECTION") {
        ret.appendString("await $events.end(async () => {\n");
      } else {
        ret.appendString(`await $events.run("${event}", async () => {\n`);
      }
      ret.appendChunk(this.blocks.get(event)!);
      ret.appendString("});\n");
    }
    ret.appendString("$events.done();\n");
    return ret;
  }
}
