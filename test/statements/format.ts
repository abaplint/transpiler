import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - FORMAT and HIDE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("FORMAT does not change the text of the list", async () => {
    const code = `
DATA lv_text TYPE c LENGTH 5 VALUE 'abc'.
FORMAT COLOR COL_HEADING INTENSIFIED ON.
WRITE / lv_text.
FORMAT COLOR = 5 INVERSE ON HOTSPOT ON INPUT OFF FRAMES ON.
WRITE / 'def'.
FORMAT RESET.
WRITE / 'ghi'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("abc  \ndef\nghi");
  });

  it("HIDE does not change the list", async () => {
    const code = `
DATA lv_text TYPE c LENGTH 5 VALUE 'abc'.
WRITE / lv_text.
HIDE lv_text.
WRITE / 'def'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("abc  \ndef");
  });

  it("obsolete list format statements and SET EXTENDED CHECK do not change the list", async () => {
    const code = `
SET EXTENDED CHECK OFF.
DETAIL.
WRITE / 'abc'.
SUMMARY.
WRITE / 'def'.
SET EXTENDED CHECK ON.
IF 1 = 2.
  PRINT-CONTROL FUNCTION 'SF000'.
ENDIF.
WRITE / 'ghi'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("abc\ndef\nghi");
  });

});
