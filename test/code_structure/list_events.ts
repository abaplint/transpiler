import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, compileFiles, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

/** runs the report, returns its list */
async function output(contents: string) {
  const f = new AsyncFunction("abap", await run(contents));
  await f(abap);
  return abap.console.get();
}

describe("Running code structure - List Events", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("simple start of selection", async () => {
    const code = `
REPORT zfoobar.

START-OF-SELECTION.
  WRITE 'hello world'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello world");
  });

  it("start + end", async () => {
    const code = `
REPORT zfoobar.

START-OF-SELECTION.
  WRITE / 'hello'.

END-OF-SELECTION.
  WRITE / 'world'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello\nworld");
  });

  it("same the other way around, start + end", async () => {
    const code = `
REPORT zfoobar.

END-OF-SELECTION.
  WRITE / 'world'.

START-OF-SELECTION.
  WRITE / 'hello'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello\nworld");
  });

  it("same the other way around, start + end, multiple statements", async () => {
    const code = `
REPORT zfoobar.

END-OF-SELECTION.
  WRITE / 'world'.
  WRITE / 'world'.

START-OF-SELECTION.
  WRITE / 'hello'.
  WRITE / 'hello'.
  `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello\nhello\nworld\nworld");
  });

  it("ignore these two", async () => {
    const code = `
REPORT zfoobar.

AT SELECTION-SCREEN.
  WRITE / 'world'.

AT LINE-SELECTION.
  WRITE / 'hello'.
  `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("the events run in the order of the runtime, not in source order", async () => {
    const code = `
REPORT zfoobar.
PARAMETERS p_value TYPE i.

END-OF-SELECTION.
  WRITE / 'end'.

START-OF-SELECTION.
  WRITE / 'start'.

AT SELECTION-SCREEN.
  WRITE / 'screen'.

AT SELECTION-SCREEN ON p_value.
  WRITE / 'screen on'.

AT SELECTION-SCREEN OUTPUT.
  WRITE / 'screen output'.

INITIALIZATION.
  WRITE / 'initialization'.

LOAD-OF-PROGRAM.
  WRITE / 'load'.`;
    expect(await output(code)).to.equal("load\ninitialization\nscreen output\nscreen on\nscreen\nstart\nend");
  });

  it("code before the first event is START-OF-SELECTION, blocks of the same event run in source order", async () => {
    const code = `
REPORT zfoobar.
WRITE / 'first'.

START-OF-SELECTION.
  WRITE / 'second'.

INITIALIZATION.
  WRITE / 'initialization'.

START-OF-SELECTION.
  WRITE / 'third'.`;
    expect(await output(code)).to.equal("initialization\nfirst\nsecond\nthird");
  });

  it("without a selection screen the AT SELECTION-SCREEN events do not run, blocks of a user's action never", async () => {
    const code = `
REPORT zfoobar.

AT SELECTION-SCREEN.
  WRITE / 'screen'.

AT LINE-SELECTION.
  WRITE / 'line'.

AT USER-COMMAND.
  WRITE / 'command'.

START-OF-SELECTION.
  WRITE / 'start'.`;
    expect(await output(code)).to.equal("start");
  });

  it("DATA in a block that does not run is still declared", async () => {
    const code = `
REPORT zfoobar.

AT LINE-SELECTION.
  DATA lv_text TYPE string.
  lv_text = 'line'.

START-OF-SELECTION.
  lv_text = 'start'.
  WRITE / lv_text.`;
    expect(await output(code)).to.equal("start");
  });

  it("RETURN in INITIALIZATION leaves INITIALIZATION only", async () => {
    const code = `
REPORT zfoobar.

INITIALIZATION.
  WRITE / 'initialization'.
  RETURN.
  WRITE / 'not here'.

START-OF-SELECTION.
  WRITE / 'start'.`;
    expect(await output(code)).to.equal("initialization\nstart");
  });

  it("RETURN in START-OF-SELECTION ends the program, END-OF-SELECTION does not run", async () => {
    const code = `
REPORT zfoobar.

START-OF-SELECTION.
  WRITE / 'start'.
  RETURN.
  WRITE / 'not here'.

END-OF-SELECTION.
  WRITE / 'end'.`;
    expect(await output(code)).to.equal("start");
  });

  it("CHECK in START-OF-SELECTION leaves it, END-OF-SELECTION runs", async () => {
    const code = `
REPORT zfoobar.
DATA lv_value TYPE i.

START-OF-SELECTION.
  WRITE / 'start'.
  CHECK lv_value = 1.
  WRITE / 'not here'.

END-OF-SELECTION.
  WRITE / 'end'.`;
    expect(await output(code)).to.equal("start\nend");
  });

  it("STOP in a FORM called from START-OF-SELECTION leaves it, END-OF-SELECTION runs", async () => {
    const code = `
REPORT zfoobar.

START-OF-SELECTION.
  WRITE / 'start'.
  PERFORM stop_here.
  WRITE / 'not here'.

END-OF-SELECTION.
  WRITE / 'end'.

FORM stop_here.
  STOP.
ENDFORM.`;
    expect(await output(code)).to.equal("start\nend");
  });

  it("STOP in INITIALIZATION skips START-OF-SELECTION, END-OF-SELECTION runs", async () => {
    const code = `
REPORT zfoobar.

INITIALIZATION.
  WRITE / 'initialization'.
  STOP.

START-OF-SELECTION.
  WRITE / 'start'.

END-OF-SELECTION.
  WRITE / 'end'.`;
    expect(await output(code)).to.equal("initialization\nend");
  });

  it("STOP in a program without events ends it", async () => {
    const code = `
REPORT zfoobar.
WRITE / 'before'.
STOP.
WRITE / 'after'.`;
    expect(await output(code)).to.equal("before");
  });

  it("TOP-OF-PAGE runs before the first list output, with the values set before it", async () => {
    const code = `
REPORT zfoobar.
DATA lv_title TYPE string.

TOP-OF-PAGE.
  WRITE / lv_title.
  ULINE.

START-OF-SELECTION.
  lv_title = 'heading'.
  PERFORM write_line.
  lv_title = 'changed'.
  WRITE / 'second'.

FORM write_line.
  WRITE / 'first'.
ENDFORM.`;
    const js = await runFiles(abap, [{filename: "zfoobar.prog.abap", contents: code}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().split("\n")).to.deep.equal(["heading", "-".repeat(50), "first", "second"]);
  });

  it("TOP-OF-PAGE does not run without list output", async () => {
    const code = `
REPORT zfoobar.
DATA lv_text TYPE c LENGTH 10.

TOP-OF-PAGE.
  WRITE / 'heading'.

START-OF-SELECTION.
  WRITE 'text' TO lv_text.`;
    expect(await output(code)).to.equal("");
  });

  it("events in an INCLUDE take their place in the program's order", async () => {
    const prog = (name: string, subc: string) => `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_PROG" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>${name}</NAME>
    <SUBC>${subc}</SUBC>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: `REPORT zfoobar.
START-OF-SELECTION.
  WRITE / 'start'.
INCLUDE zfoobar_events.`},
      {filename: "zfoobar.prog.xml", contents: prog("ZFOOBAR", "1")},
      {filename: "zfoobar_events.prog.abap", contents: `INITIALIZATION.
  WRITE / 'initialization'.
END-OF-SELECTION.
  WRITE / 'end'.`},
      {filename: "zfoobar_events.prog.xml", contents: prog("ZFOOBAR_EVENTS", "I")},
    ]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("initialization\nstart\nend");
  });

  it("a program module runs its events from $main", async () => {
    const res = await compileFiles([{filename: "zfoobar.prog.abap", contents: `REPORT zfoobar.
INITIALIZATION.
  WRITE / 'initialization'.
START-OF-SELECTION.
  WRITE / 'start'.`}], {addCommonJS: true});
    const prog = res.objects.find(o => o.object.name === "ZFOOBAR");
    expect(prog!.exports).to.include("$main");
    const code = prog!.chunk.getCode();
    expect(code.indexOf("async function $main() {")).to.be.lessThan(code.indexOf("abap.statements.reportEvents()"));
  });

  it("F4 and F1 help, the exit command and TOP-OF-PAGE DURING LINE-SELECTION do not run", async () => {
    const code = `
REPORT zfoobar.
PARAMETERS p_value TYPE i.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_value.
  WRITE / 'value request'.

AT SELECTION-SCREEN ON HELP-REQUEST FOR p_value.
  WRITE / 'help request'.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  WRITE / 'exit command'.

AT SELECTION-SCREEN ON p_value.
  WRITE / 'screen on'.

TOP-OF-PAGE DURING LINE-SELECTION.
  WRITE / 'heading of a details list'.

START-OF-SELECTION.
  WRITE / 'start'.`;
    expect(await output(code)).to.equal("screen on\nstart");
  });
});