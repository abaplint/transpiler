import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

async function runLines(code: string): Promise<string[]> {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  await f(abap);
  return abap.console.get().split("\n");
}

class Mismatch extends Error {
  public async constructor_() {
    return this;
  }
}

describe("Running statements - EXPORT, IMPORT, DELETE FROM MEMORY, FREE MEMORY", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("EXPORT TO MEMORY replaces the cluster, IMPORT keeps targets of missing parameters", async () => {
    const lines = await runLines(`
DATA lv_a TYPE i VALUE 1.
DATA lv_b TYPE c LENGTH 1 VALUE 'X'.
DATA lv_c TYPE i VALUE 3.
EXPORT pa = lv_a pb FROM lv_b TO MEMORY ID 'ZID1'.
EXPORT pc = lv_c TO MEMORY ID 'ZID1'.
lv_a = 99.
lv_b = 'Y'.
IMPORT pa = lv_a pb = lv_b FROM MEMORY ID 'ZID1'.
WRITE / |{ sy-subrc } { lv_a } { lv_b }|.
lv_c = 0.
IMPORT pc = lv_c FROM MEMORY ID 'ZID1'.
WRITE / |{ sy-subrc } { lv_c }|.
IMPORT pa = lv_a FROM MEMORY ID 'ZNONE'.
WRITE / |{ sy-subrc } { lv_a }|.`);
    expect(lines).to.deep.equal(["0 99 Y", "0 3", "4 99"]);
  });

  it("DELETE FROM MEMORY and FREE MEMORY", async () => {
    const lines = await runLines(`
DATA lv_a TYPE i VALUE 5.
EXPORT pa = lv_a TO MEMORY ID 'ZID1'.
EXPORT pa = lv_a TO MEMORY ID 'ZID2'.
EXPORT pa = lv_a TO MEMORY ID 'ZID3'.
DELETE FROM MEMORY ID 'ZID1'.
WRITE / sy-subrc.
DELETE FROM MEMORY ID 'ZID1'.
WRITE / sy-subrc.
FREE MEMORY ID 'ZID2'.
IMPORT pa = lv_a FROM MEMORY ID 'ZID2'.
WRITE / sy-subrc.
FREE MEMORY.
IMPORT pa = lv_a FROM MEMORY ID 'ZID3'.
WRITE / sy-subrc.`);
    expect(lines).to.deep.equal(["0", "4", "4", "4"]);
  });

  it("DATA BUFFER, the type rules of IMPORT", async () => {
    const lines = await runLines(`
TYPES: BEGIN OF ty_s1,
         a TYPE i,
         b TYPE c LENGTH 3,
       END OF ty_s1,
       BEGIN OF ty_s2,
         a TYPE i,
         b TYPE c LENGTH 3,
         c TYPE i,
       END OF ty_s2.
DATA lv_buf TYPE xstring.
DATA lv_c3 TYPE c LENGTH 3.
DATA lv_c5 TYPE c LENGTH 5 VALUE 'ABCDE'.
DATA lv_c8 TYPE c LENGTH 8 VALUE 'ZZZZZZZZ'.
DATA ls_s1 TYPE ty_s1.
DATA ls_s2 TYPE ty_s2.
  EXPORT p = lv_c5 TO DATA BUFFER lv_buf.
  IMPORT p = lv_c3 FROM DATA BUFFER lv_buf.
  WRITE / lv_c3.
  IMPORT p = lv_c8 FROM DATA BUFFER lv_buf.
  WRITE / |{ lv_c8 }.|.
  ls_s1-a = 7.
  ls_s1-b = 'abc'.
  EXPORT s = ls_s1 TO DATA BUFFER lv_buf.
  ls_s2-c = 42.
  IMPORT s = ls_s2 FROM DATA BUFFER lv_buf.
  WRITE / |{ ls_s2-a }/{ ls_s2-b }/{ ls_s2-c }|.
`);
    expect(lines).to.deep.equal(["ABC", "ABCDE.", "7/abc/0"]);
  });

  it("a table with header line exports and imports its rows, the header line keeps its value", async () => {
    const lines = await runLines(`
TYPES ty_c1 TYPE c LENGTH 1.
DATA lt_hl TYPE STANDARD TABLE OF ty_c1 WITH HEADER LINE.
DATA lv_rows TYPE i.
DATA lv_header TYPE c LENGTH 1.
lt_hl = 'A'.
APPEND lt_hl.
lt_hl = 'B'.
APPEND lt_hl.
lt_hl = 'Z'.
EXPORT t = lt_hl TO MEMORY ID 'ZID1'.
CLEAR lt_hl[].
lt_hl = 'Q'.
IMPORT t = lt_hl FROM MEMORY ID 'ZID1'.
lv_rows = lines( lt_hl[] ).
lv_header = lt_hl.
WRITE / |{ lv_rows } { lv_header }|.`);
    expect(lines).to.deep.equal(["2 Q"]);
  });

  it("IMPORT into a data object of another type raises CX_SY_IMPORT_MISMATCH_ERROR", async () => {
    abap.Classes["CX_SY_IMPORT_MISMATCH_ERROR"] = Mismatch;
    const js = await run(`
DATA lv_buf TYPE xstring.
DATA lv_i TYPE i VALUE 12.
DATA lv_c5 TYPE c LENGTH 5.
EXPORT p = lv_i TO DATA BUFFER lv_buf.
IMPORT p = lv_c5 FROM DATA BUFFER lv_buf.
WRITE / 'after'.`);
    let caught: any = undefined;
    try {
      await new AsyncFunction("abap", js)(abap);
    } catch (e) {
      caught = e;
    }
    expect(caught).to.be.instanceof(Mismatch);
    expect(abap.console.get()).to.equal("");
  });

  it("IMPORT from an initial byte string ends the program", async () => {
    const js = await run(`
DATA lv_buf TYPE xstring.
DATA lv_c TYPE c LENGTH 3.
IMPORT p = lv_c FROM DATA BUFFER lv_buf.
WRITE / 'after'.`);
    let error = "";
    try {
      await new AsyncFunction("abap", js)(abap);
    } catch (e) {
      error = (e as Error).message;
    }
    expect(error).to.contain("IMPORT_FROM_DATA_BUFFER_EMPTY");
    expect(abap.console.get()).to.equal("");
  });

});
