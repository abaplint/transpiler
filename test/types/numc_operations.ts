import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {expect} from "chai";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

function exceptionClass(name: string, superclass?: string) {
  return `CLASS ${name} DEFINITION PUBLIC${superclass ? " INHERITING FROM " + superclass : ""}.
ENDCLASS.
CLASS ${name} IMPLEMENTATION.
ENDCLASS.`;
}

async function run(contents: string) {
  // the program first: runFiles evaluates the first object only, so the
  // exception classes are in the registry for the syntax check and stand in
  // as plain JavaScript classes at run time
  const js = await runFiles(abap, [
    {filename: "zfoobar.prog.abap", contents},
    {filename: "cx_root.clas.abap", contents: exceptionClass("cx_root")},
    {filename: "cx_sy_zerodivide.clas.abap", contents: exceptionClass("cx_sy_zerodivide", "cx_root")},
    {filename: "cx_sy_conversion_overflow.clas.abap", contents: exceptionClass("cx_sy_conversion_overflow", "cx_root")}]);
  abap.Classes["CX_ROOT"] = class CxRoot {};
  abap.Classes["CX_SY_ZERODIVIDE"] = class CxSyZerodivide extends abap.Classes["CX_ROOT"] {};
  abap.Classes["CX_SY_CONVERSION_OVERFLOW"] = class CxSyConversionOverflow extends abap.Classes["CX_ROOT"] {};
  const f = new AsyncFunction("abap", js);
  await f(abap);
}

// Operations on n, each snippet run on a system (ABAP 816) with the same ASSERTs
describe("Running Examples - n operations", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("from text: letters and signs dropped, the right end kept", async () => {
    await run(`
DATA lv_t TYPE c LENGTH 30.
DATA lv_n TYPE n LENGTH 5.
DATA lv_n20 TYPE n LENGTH 20.
lv_t = 'a1b2c'.
lv_n = lv_t.
ASSERT lv_n = '00012'.
lv_t = '5-'.
lv_n = lv_t.
ASSERT lv_n = '00005'.
lv_t = '+7'.
lv_n = lv_t.
ASSERT lv_n = '00007'.
lv_t = '-0'.
lv_n = lv_t.
ASSERT lv_n = '00000'.
lv_t = 'abc'.
lv_n = lv_t.
ASSERT lv_n = '00000'.
lv_t = '  7'.
lv_n = lv_t.
ASSERT lv_n = '00007'.
lv_t = '1E3'.
lv_n = lv_t.
ASSERT lv_n = '00013'.
lv_t = '1234567'.
lv_n = lv_t.
ASSERT lv_n = '34567'.
lv_t = '123456789012345678901234'.
lv_n20 = lv_t.
ASSERT lv_n20 = '56789012345678901234'.`);
  });

  it("from i, int8, f and n: the absolute value, the right end kept", async () => {
    await run(`
DATA lv_i TYPE i.
DATA lv_i8 TYPE int8.
DATA lv_f TYPE f.
DATA lv_n3 TYPE n LENGTH 3.
DATA lv_n5 TYPE n LENGTH 5.
DATA lv_n20 TYPE n LENGTH 20.
lv_i = 12345.
lv_n3 = lv_i.
ASSERT lv_n3 = '345'.
lv_i8 = -12.
lv_n3 = lv_i8.
ASSERT lv_n3 = '012'.
lv_f = '2.4'.
lv_n3 = lv_f.
ASSERT lv_n3 = '002'.
lv_n3 = 0.
ASSERT lv_n3 = '000'.
lv_i = 7.
lv_n20 = lv_i.
ASSERT lv_n20 = '00000000000000000007'.
lv_n5 = '12345'.
lv_n3 = lv_n5.
ASSERT lv_n3 = '345'.
lv_n3 = '042'.
lv_n5 = lv_n3.
ASSERT lv_n5 = '00042'.`);
  });

  it("to i, int8, p, f, decfloat34, c and string", async () => {
    await run(`
DATA lv_n TYPE n LENGTH 5 VALUE '00123'.
DATA lv_n20 TYPE n LENGTH 20.
DATA lv_i TYPE i.
DATA lv_i8 TYPE int8.
DATA lv_p TYPE p LENGTH 10 DECIMALS 2.
DATA lv_p16 TYPE p LENGTH 16 DECIMALS 0.
DATA lv_f TYPE f.
DATA lv_d34 TYPE decfloat34.
DATA lv_c2 TYPE c LENGTH 2.
DATA lv_c10 TYPE c LENGTH 10.
DATA lv_s TYPE string.
lv_i = lv_n.
ASSERT lv_i = 123.
lv_p = lv_n.
ASSERT lv_p = '123.00'.
lv_f = lv_n.
ASSERT lv_f = 123.
lv_d34 = lv_n.
ASSERT lv_d34 = 123.
lv_c2 = lv_n.
ASSERT lv_c2 = '00'.
lv_c10 = lv_n.
ASSERT lv_c10 = '00123'.
lv_s = lv_n.
ASSERT lv_s = \`00123\`.
lv_n20 = '09223372036854775807'.
lv_i8 = lv_n20.
ASSERT lv_i8 = 9223372036854775807.
lv_n20 = '12345678901234567890'.
lv_p16 = lv_n20.
ASSERT lv_p16 = '12345678901234567890'.`);
  });

  it("arithmetic: calculation type p, the result rounded into i, the left end cut off in n", async () => {
    await run(`
DATA lv_n3 TYPE n LENGTH 3.
DATA lv_n5 TYPE n LENGTH 5.
DATA lv_n20 TYPE n LENGTH 20.
DATA lv_i TYPE i.
DATA lv_p TYPE p LENGTH 10 DECIMALS 2.
lv_n3 = '099'.
lv_n3 = lv_n3 + 1.
ASSERT lv_n3 = '100'.
lv_n3 = '999'.
lv_n3 = lv_n3 + 1.
ASSERT lv_n3 = '000'.
lv_n20 = '99999999999999999999'.
lv_n20 = lv_n20 + 1.
ASSERT lv_n20 = '00000000000000000000'.
lv_n3 = '007'.
lv_i = lv_n3 / 2.
ASSERT lv_i = 4.
lv_p = lv_n3 / 2.
ASSERT lv_p = '3.50'.
lv_n5 = '00007'.
lv_n3 = lv_n5 DIV 2.
ASSERT lv_n3 = '003'.
lv_n3 = lv_n5 MOD 2.
ASSERT lv_n3 = '001'.
lv_n3 = '005'.
lv_n3 = lv_n3 * lv_n3.
ASSERT lv_n3 = '025'.
lv_n3 = '002'.
lv_n3 = lv_n3 ** 3.
ASSERT lv_n3 = '008'.
lv_n3 = '010'.
lv_n5 = '00003'.
lv_p = lv_n3 / lv_n5.
ASSERT lv_p = '3.33'.
lv_i = lv_n3 * '1.5'.
ASSERT lv_i = 15.
ADD 5 TO lv_n3.
ASSERT lv_n3 = '015'.`);
  });

  it("division by zero and a p target too short", async () => {
    await run(`
DATA lv_n TYPE n LENGTH 5 VALUE '12345'.
DATA lv_zero TYPE n LENGTH 3.
DATA lv_i TYPE i VALUE 7.
DATA lv_p2 TYPE p LENGTH 2 DECIMALS 0.
DATA lv_caught TYPE abap_bool.
TRY.
    lv_i = lv_n / lv_zero.
  CATCH cx_sy_zerodivide.
    lv_caught = abap_true.
ENDTRY.
ASSERT lv_caught = abap_true.
ASSERT lv_i = 7.
lv_caught = abap_false.
TRY.
    lv_p2 = lv_n.
  CATCH cx_sy_conversion_overflow.
    lv_caught = abap_true.
ENDTRY.
ASSERT lv_caught = abap_true.`);
  });

  it("compare with i, p, f, a string of the same digits, BETWEEN and CO", async () => {
    await run(`
DATA lv_n TYPE n LENGTH 5 VALUE '00010'.
DATA lv_p TYPE p LENGTH 10 DECIMALS 2 VALUE '10.5'.
DATA lv_f TYPE f VALUE '9.5'.
ASSERT lv_n = 10.
ASSERT lv_n = \`00010\`.
ASSERT lv_n < lv_p.
ASSERT lv_n > lv_f.
ASSERT lv_n BETWEEN 5 AND 10.
ASSERT lv_n CO '0123456789'.`);
  });

  it("compare with c, string and n of another length: as numbers", async () => {
    await run(`
DATA lv_n TYPE n LENGTH 5 VALUE '00010'.
DATA lv_n3 TYPE n LENGTH 3 VALUE '010'.
DATA lv_s TYPE string VALUE \`9\`.
ASSERT lv_n = '10'.
ASSERT lv_n = '010'.
ASSERT lv_n > '9'.
ASSERT lv_n > lv_s.
ASSERT lv_n = ' 10'.
ASSERT lv_n3 = lv_n.`);
  });

  it("compare with a text that is no number: CONVT_NO_NUMBER", async () => {
    try {
      await run(`
DATA lv_n TYPE n LENGTH 5 VALUE '00010'.
IF lv_n = 'x10'.
ENDIF.`);
      expect.fail();
    } catch (e) {
      expect((e as Error).toString()).to.contain("CONVT_NO_NUMBER");
    }
  });

  it("READ TABLE and table expression: a text key is converted to the n component", async () => {
    await run(`
TYPES: BEGIN OF ty, k TYPE n LENGTH 5, END OF ty.
DATA lt TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA lt_s TYPE SORTED TABLE OF ty WITH UNIQUE KEY k.
DATA lt_h TYPE HASHED TABLE OF ty WITH UNIQUE KEY k.
DATA ls TYPE ty.
ls-k = '00010'.
APPEND ls TO lt.
INSERT ls INTO TABLE lt_s.
INSERT ls INTO TABLE lt_h.
CLEAR ls.
READ TABLE lt INTO ls WITH KEY k = 'x10'.
ASSERT sy-subrc = 0.
ASSERT ls-k = '00010'.
READ TABLE lt_s INTO ls WITH KEY k = 'x10'.
ASSERT sy-subrc = 0.
READ TABLE lt_s INTO ls WITH TABLE KEY k = 'x10'.
ASSERT sy-subrc = 0.
READ TABLE lt_h INTO ls WITH TABLE KEY k = 'x10'.
ASSERT sy-subrc = 0.
CLEAR ls.
ls = VALUE #( lt[ k = 'x10' ] OPTIONAL ).
ASSERT ls-k = '00010'.`);
  });

  it("LOOP WHERE with a text that is no number: CONVT_NO_NUMBER", async () => {
    try {
      await run(`
TYPES: BEGIN OF ty, k TYPE n LENGTH 5, END OF ty.
DATA lt TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA ls TYPE ty.
ls-k = '00010'.
APPEND ls TO lt.
LOOP AT lt INTO ls WHERE k = 'x10'.
ENDLOOP.`);
      expect.fail();
    } catch (e) {
      expect((e as Error).toString()).to.contain("CONVT_NO_NUMBER");
    }
  });

  it("string templates", async () => {
    await run(`
DATA lv_n TYPE n LENGTH 5 VALUE '00123'.
DATA lv_n3 TYPE n LENGTH 3 VALUE '012'.
ASSERT |[{ lv_n }]| = \`[00123]\`.
ASSERT |[{ lv_n WIDTH = 8 }]| = \`[00123   ]\`.
ASSERT |[{ lv_n PAD = 'x' WIDTH = 7 }]| = \`[00123xx]\`.
ASSERT |[{ lv_n3 ALPHA = IN }]| = \`[012]\`.`);
  });

  it("strlen, CLEAR, offset read and write at the end, CONCATENATE, &&, condense, SORT", async () => {
    await run(`
DATA lv_n TYPE n LENGTH 5 VALUE '00123'.
DATA lv_n3 TYPE n LENGTH 3.
DATA lv_i TYPE i.
DATA lv_s TYPE string.
DATA lt_n LIKE STANDARD TABLE OF lv_n3 WITH DEFAULT KEY.
ASSERT strlen( lv_n ) = 5.
ASSERT numofchar( lv_n ) = 5.
ASSERT condense( lv_n ) = \`00123\`.
lv_s = lv_n+3.
ASSERT lv_s = \`23\`.
lv_i = lv_n+3(2).
ASSERT lv_i = 23.
CONCATENATE lv_n 'x' INTO lv_s.
ASSERT lv_s = \`00123x\`.
lv_n = lv_n && '9'.
ASSERT lv_n = '01239'.
lv_n = '00123'.
lv_n+3 = '45'.
ASSERT lv_n = '00145'.
CLEAR lv_n.
ASSERT lv_n = '00000'.
ASSERT lv_n IS INITIAL.
APPEND '010' TO lt_n.
APPEND '002' TO lt_n.
APPEND '100' TO lt_n.
SORT lt_n DESCENDING.
lv_s = concat_lines_of( table = lt_n sep = \`,\` ).
ASSERT lv_s = \`100,010,002\`.`);
  });

});
