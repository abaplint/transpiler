import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  const js = await runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
  const f = new AsyncFunction("abap", js);
  await f(abap);
  return abap.console.get();
}

// expectations measured on a 7.5x system: the report ran as a background job
// step, so no selection screen was shown and only the defaults applied
describe("Running statements - PARAMETERS and SELECT-OPTIONS", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("PARAMETERS, initial without DEFAULT", async () => {
    const code = `
    PARAMETERS p_plain.
    PARAMETERS p_num TYPE i.
    WRITE |<{ p_plain }>{ p_num }|.`;
    expect(await run(code)).to.equal("<>0");
  });

  it("PARAMETERS, DEFAULT constant and field", async () => {
    const code = `
    PARAMETERS p_num TYPE i DEFAULT 5.
    PARAMETERS p_dat TYPE d DEFAULT sy-datum.
    WRITE |{ p_num } { boolc( p_dat = sy-datum AND p_dat IS NOT INITIAL ) }|.`;
    expect(await run(code)).to.equal("5 X");
  });

  it("PARAMETERS, a character DEFAULT is cut to length and upper-cased", async () => {
    const code = `
    PARAMETERS p_txt(4) DEFAULT 'abcdef'.
    PARAMETERS p_str TYPE string DEFAULT 'mixed Case'.
    WRITE |<{ p_txt }><{ p_str }>|.`;
    expect(await run(code)).to.equal("<ABCD><MIXED CASE>");
  });

  it("PARAMETERS, LOWER CASE keeps the case", async () => {
    const code = `
    PARAMETERS p_low(4) LOWER CASE DEFAULT 'abcdef'.
    WRITE |<{ p_low }>|.`;
    expect(await run(code)).to.equal("<abcd>");
  });

  it("PARAMETERS, checkboxes", async () => {
    const code = `
    PARAMETERS p_cb AS CHECKBOX.
    PARAMETERS p_cd AS CHECKBOX DEFAULT 'X'.
    WRITE |<{ p_cb }><{ p_cd }>|.`;
    expect(await run(code)).to.equal("<><X>");
  });

  it("PARAMETERS, a radio button group starts on its first button unless one has DEFAULT 'X'", async () => {
    const code = `
    PARAMETERS p_r1 RADIOBUTTON GROUP g1.
    PARAMETERS p_r2 RADIOBUTTON GROUP g1.
    PARAMETERS p_q1 RADIOBUTTON GROUP g2.
    PARAMETERS p_q2 RADIOBUTTON GROUP g2 DEFAULT 'X'.
    WRITE |{ p_r1 }-{ p_r2 }-{ p_q1 }-{ p_q2 }|.`;
    expect(await run(code)).to.equal("X---X");
  });

  it("SELECT-OPTIONS, empty without DEFAULT", async () => {
    const code = `
    DATA gi TYPE i.
    SELECT-OPTIONS s_d FOR gi.
    WRITE |{ lines( s_d ) } <{ s_d-sign }{ s_d-option }{ s_d-low }>|.`;
    expect(await run(code)).to.equal("0 <0>");
  });

  it("SELECT-OPTIONS, DEFAULT is I EQ, in the table and in the header line", async () => {
    const code = `
    DATA gv TYPE c LENGTH 10.
    SELECT-OPTIONS s_a FOR gv DEFAULT 'A1'.
    DATA ls LIKE LINE OF s_a.
    READ TABLE s_a INDEX 1 INTO ls.
    WRITE |{ lines( s_a ) } { ls-sign }{ ls-option }{ ls-low } { s_a-sign }{ s_a-option }{ s_a-low }|.`;
    expect(await run(code)).to.equal("1 IEQA1 IEQA1");
  });

  it("SELECT-OPTIONS, DEFAULT with TO is I BT", async () => {
    const code = `
    DATA gv TYPE c LENGTH 10.
    SELECT-OPTIONS s_b FOR gv DEFAULT 'B1' TO 'B9'.
    WRITE |{ lines( s_b ) } { s_b-sign }{ s_b-option }{ s_b-low }-{ s_b-high }|.`;
    expect(await run(code)).to.equal("1 IBTB1-B9");
  });

  it("SELECT-OPTIONS, OPTION and SIGN", async () => {
    const code = `
    DATA gv TYPE c LENGTH 10.
    SELECT-OPTIONS s_c FOR gv DEFAULT 'C*' OPTION CP SIGN E.
    WRITE |{ s_c-sign }{ s_c-option }{ s_c-low }|.`;
    expect(await run(code)).to.equal("ECPC*");
  });

  it("SELECT-OPTIONS, the LOW of a default is upper-cased unless LOWER CASE", async () => {
    const code = `
    DATA gv TYPE c LENGTH 10.
    SELECT-OPTIONS s_l FOR gv DEFAULT 'ab'.
    SELECT-OPTIONS s_m FOR gv DEFAULT 'ab' LOWER CASE.
    WRITE |{ s_l-low } { s_m-low }|.`;
    expect(await run(code)).to.equal("AB ab");
  });

  it("SELECT-OPTIONS, an integer default and IN", async () => {
    const code = `
    DATA gi TYPE i.
    SELECT-OPTIONS s_e FOR gi DEFAULT 3 NO INTERVALS NO-EXTENSION.
    WRITE |{ s_e-option }{ s_e-low } { boolc( 3 IN s_e ) }{ boolc( 4 IN s_e ) }|.`;
    expect(await run(code)).to.equal("EQ3 X ");
  });

  it("PARAMETERS, an operand named like a keyword is an operand", async () => {
    const code = `
    CONSTANTS sign TYPE c LENGTH 1 VALUE 'E'.
    CONSTANTS to TYPE c LENGTH 2 VALUE 'Z9'.
    DATA gv TYPE c LENGTH 10.
    SELECT-OPTIONS s_s FOR gv DEFAULT sign.
    SELECT-OPTIONS s_t FOR gv DEFAULT 'A1' TO to.
    PARAMETERS p_s(1) DEFAULT sign.
    WRITE |{ s_s-sign }{ s_s-option }{ s_s-low } { s_t-option }{ s_t-low }-{ s_t-high } { p_s }|.`;
    expect(await run(code)).to.equal("IEQE BTA1-Z9 E");
  });

  it("PARAMETERS, a number into a character parameter, and LOWER CASE inside a literal", async () => {
    const code = `
    PARAMETERS p_c(3) DEFAULT 1.
    PARAMETERS p_l(12) DEFAULT 'a lower case'.
    WRITE |<{ p_c }><{ p_l }>|.`;
    expect(await run(code)).to.equal("< 1><A LOWER CASE>");
  });

  it("PARAMETERS with LENGTH or DECIMALS is not supported yet", async () => {
    const code = `
    PARAMETERS p_c TYPE c LENGTH 3 DEFAULT 'abc'.
    WRITE p_c.`;
    let message = "";
    try {
      await run(code);
    } catch (e: any) {
      message = e.message;
    }
    expect(message).to.contain("not supported");
  });

});
