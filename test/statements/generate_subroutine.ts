import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar_generate.prog.abap", contents}]);
}

describe("Running statements - GENERATE SUBROUTINE POOL", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("refuses with sy-subrc 8 and a message, no exception", async () => {
    const code = `
DATA lt_src TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lv_prog TYPE c LENGTH 40.
DATA lv_msg TYPE string.
DATA lv_line TYPE i.
DATA lv_word TYPE string.
lv_prog = 'UNSET'.
lv_msg = 'UNSET'.
lv_line = 99.
lv_word = 'UNSET'.
sy-subrc = 99.
APPEND |PROGRAM.| TO lt_src.
GENERATE SUBROUTINE POOL lt_src NAME lv_prog MESSAGE lv_msg LINE lv_line WORD lv_word.
WRITE |{ sy-subrc }:{ lv_prog }:{ lv_msg }:{ lv_line }:{ lv_word }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("8::GENERATE SUBROUTINE POOL is not supported:0:");
  });

  it("only NAME", async () => {
    const code = `
DATA lt_src TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lv_prog TYPE c LENGTH 40.
lv_prog = 'UNSET'.
GENERATE SUBROUTINE POOL lt_src NAME lv_prog.
IF sy-subrc <> 0.
  WRITE |{ sy-subrc }:{ lv_prog }|.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("8:");
  });

  it("other additions compile and are left untouched", async () => {
    const code = `
DATA lt_src TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lv_prog TYPE c LENGTH 40.
DATA lv_msg TYPE string.
DATA lv_msgid TYPE string.
DATA lv_offset TYPE i.
DATA lv_incl TYPE c LENGTH 40.
DATA lv_dump TYPE string.
lv_msgid = 'ID'.
lv_offset = 5.
lv_incl = 'INCL'.
lv_dump = 'DUMP'.
GENERATE SUBROUTINE POOL lt_src NAME lv_prog MESSAGE-ID lv_msgid MESSAGE lv_msg
  OFFSET lv_offset INCLUDE lv_incl SHORTDUMP-ID lv_dump.
WRITE |{ sy-subrc }:{ lv_msg }:{ lv_msgid }:{ lv_offset }:{ lv_incl }:{ lv_dump }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("8:GENERATE SUBROUTINE POOL is not supported:ID:5:INCL:DUMP");
  });

});
