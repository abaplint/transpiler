import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

async function runAndGet(code: string) {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  await f(abap);
  return abap.console.get();
}

describe("Running types - decfloat16", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("declares, assigns and compares", async () => {
    const code = `
    DATA lv TYPE decfloat16.
    DATA lv_limit TYPE decfloat16.
    lv_limit = 30 * 60.
    lv = '1800.5'.
    IF lv > lv_limit.
      WRITE / 'greater'.
    ENDIF.
    lv = lv - lv_limit.
    WRITE / lv.`;
    expect(await runAndGet(code)).to.equal("greater\n0,5");
  });

  it("a decfloat16 local of a method, the shape of a session expiry check", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS expired IMPORTING iv_age TYPE i RETURNING VALUE(rv) TYPE abap_bool.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD expired.
    DATA lv_expiry_seconds TYPE decfloat16.
    lv_expiry_seconds = 30 * 60.
    rv = boolc( iv_age > lv_expiry_seconds ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  WRITE / lcl=>expired( 1801 ).
  WRITE / lcl=>expired( 1800 ).`;
    expect(await runAndGet(code)).to.equal("X\n ");
  });

  it("CONV decfloat16 of a difference of timestamps", async () => {
    const code = `
    DATA lv_start TYPE p LENGTH 11 DECIMALS 7 VALUE '20261118100000.25'.
    DATA lv_end TYPE p LENGTH 11 DECIMALS 7 VALUE '20261118100002.75'.
    DATA lv_duration TYPE decfloat16.
    lv_duration = CONV decfloat16( lv_end - lv_start ).
    WRITE / lv_duration.`;
    expect(await runAndGet(code)).to.equal("2,5");
  });

});
