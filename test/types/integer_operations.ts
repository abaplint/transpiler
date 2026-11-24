import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
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
    {filename: "cx_sy_conversion_no_number.clas.abap", contents: exceptionClass("cx_sy_conversion_no_number", "cx_root")}]);
  abap.Classes["CX_ROOT"] = class CxRoot {};
  abap.Classes["CX_SY_ZERODIVIDE"] = class CxSyZerodivide extends abap.Classes["CX_ROOT"] {};
  abap.Classes["CX_SY_CONVERSION_NO_NUMBER"] = class CxSyConversionNoNumber extends abap.Classes["CX_ROOT"] {};
  const f = new AsyncFunction("abap", js);
  await f(abap);
}

// Operations on i, each snippet run on a system (ABAP 816) with the same ASSERTs
describe("Running Examples - i operations", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("/ rounds half away from zero, DIV and MOD with a negative divisor", async () => {
    await run(`
DATA lv_a TYPE i.
DATA lv_b TYPE i.
DATA lv_r TYPE i.
lv_a = 7.
lv_b = 2.
lv_r = lv_a / lv_b.
ASSERT lv_r = 4.
lv_a = -7.
lv_r = lv_a / lv_b.
ASSERT lv_r = -4.
lv_a = 5.
lv_r = lv_a / lv_b.
ASSERT lv_r = 3.
lv_a = -5.
lv_r = lv_a / lv_b.
ASSERT lv_r = -3.
lv_a = 7.
lv_b = -2.
lv_r = lv_a DIV lv_b.
ASSERT lv_r = -3.
lv_r = lv_a MOD lv_b.
ASSERT lv_r = 1.`);
  });

  it("**, ipow, abs, sign, nmax and nmin", async () => {
    await run(`
DATA lv_a TYPE i.
DATA lv_r TYPE i.
lv_a = 2.
lv_r = lv_a ** 10.
ASSERT lv_r = 1024.
lv_r = ipow( base = lv_a exp = 10 ).
ASSERT lv_r = 1024.
lv_r = ipow( base = lv_a exp = 0 ).
ASSERT lv_r = 1.
lv_a = -3.
lv_r = ipow( base = lv_a exp = 3 ).
ASSERT lv_r = -27.
lv_a = -15.
lv_r = abs( lv_a ).
ASSERT lv_r = 15.
lv_r = sign( lv_a ).
ASSERT lv_r = -1.
lv_r = nmax( val1 = lv_a val2 = 3 val3 = 7 ).
ASSERT lv_r = 7.
lv_r = nmin( val1 = lv_a val2 = 3 val3 = 7 ).
ASSERT lv_r = -15.`);
  });

  it("division by zero is CX_SY_ZERODIVIDE and keeps the target, 0 / 0 is 0, text that is no number, also 1E3", async () => {
    await run(`
DATA lv_a TYPE i VALUE 7.
DATA lv_b TYPE i.
DATA lv_r TYPE i VALUE 5.
DATA lv_count TYPE i.
TRY.
    lv_r = lv_a / lv_b.
  CATCH cx_sy_zerodivide.
    lv_count = lv_count + 1.
ENDTRY.
ASSERT lv_r = 5.
TRY.
    lv_r = lv_a DIV lv_b.
  CATCH cx_sy_zerodivide.
    lv_count = lv_count + 1.
ENDTRY.
TRY.
    lv_r = lv_a MOD lv_b.
  CATCH cx_sy_zerodivide.
    lv_count = lv_count + 1.
ENDTRY.
ASSERT lv_count = 3.
CLEAR lv_a.
lv_r = lv_a / lv_b.
ASSERT lv_r = 0.
TRY.
    lv_r = 'abc'.
  CATCH cx_sy_conversion_no_number.
    lv_count = lv_count + 1.
ENDTRY.
TRY.
    lv_r = '1E3'.
  CATCH cx_sy_conversion_no_number.
    lv_count = lv_count + 1.
ENDTRY.
ASSERT lv_count = 5.`);
  });

  it("from text: sign last, blanks, decimals rounded half away from zero, plus sign, empty", async () => {
    await run(`
DATA lv_r TYPE i.
lv_r = '12-'.
ASSERT lv_r = -12.
lv_r = ' 12 '.
ASSERT lv_r = 12.
lv_r = '2.5'.
ASSERT lv_r = 3.
lv_r = '-2.5'.
ASSERT lv_r = -3.
lv_r = '+7'.
ASSERT lv_r = 7.
lv_r = ''.
ASSERT lv_r = 0.`);
  });

  it("to c with the sign last, string, f, p, decfloat34, and to a short n", async () => {
    await run(`
DATA lv_a TYPE i VALUE -15.
DATA lv_c TYPE c LENGTH 6.
DATA lv_n TYPE n LENGTH 4.
DATA lv_s TYPE string.
DATA lv_f TYPE f.
DATA lv_p TYPE p LENGTH 8 DECIMALS 2.
DATA lv_d TYPE decfloat34.
lv_c = lv_a.
ASSERT lv_c = '   15-'.
lv_s = lv_a.
ASSERT lv_s = \`15-\`.
lv_f = lv_a.
ASSERT lv_f = -15.
ASSERT |{ lv_f STYLE = SCIENTIFIC }| = \`-1.5E+01\`.
lv_p = lv_a.
ASSERT |{ lv_p }| = \`-15.00\`.
lv_d = lv_a.
ASSERT |{ lv_d }| = \`-15\`.
lv_a = 123456.
lv_n = lv_a.
ASSERT lv_n = '3456'.`);
  });

  it("string templates: DECIMALS, WIDTH, NUMBER = RAW, STYLE = SIMPLE, SIGN = LEFT, ALIGN = LEFT, PAD", async () => {
    await run(`
DATA lv_a TYPE i VALUE -1234.
ASSERT |{ lv_a }| = \`-1234\`.
ASSERT |{ lv_a DECIMALS = 2 }| = \`-1234.00\`.
ASSERT |[{ lv_a WIDTH = 8 }]| = \`[-1234   ]\`.
ASSERT |{ lv_a NUMBER = RAW }| = \`-1234\`.
ASSERT |{ lv_a STYLE = SIMPLE }| = \`-1234\`.
ASSERT |{ lv_a SIGN = LEFT }| = \`-1234\`.
ASSERT |[{ lv_a ALIGN = LEFT WIDTH = 8 }]| = \`[-1234   ]\`.
ASSERT |[{ lv_a PAD = '0' WIDTH = 8 ALIGN = RIGHT }]| = \`[000-1234]\`.`);
  });

  it("COLLECT sums, SORT and READ TABLE WITH KEY on an i component", async () => {
    await run(`
TYPES: BEGIN OF ty_row,
         key    TYPE c LENGTH 1,
         amount TYPE i,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls_row TYPE ty_row.
DATA lv_keys TYPE string.
ls_row-key = 'B'.
ls_row-amount = -11.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'A'.
ls_row-amount = 22.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'B'.
ls_row-amount = 4.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'C'.
ls_row-amount = -30.
COLLECT ls_row INTO lt_rows.
ASSERT lines( lt_rows ) = 3.
SORT lt_rows BY amount.
LOOP AT lt_rows INTO ls_row.
  lv_keys = lv_keys && ls_row-key.
ENDLOOP.
ASSERT lv_keys = \`CBA\`.
CLEAR lv_keys.
SORT lt_rows BY amount DESCENDING.
LOOP AT lt_rows INTO ls_row.
  lv_keys = lv_keys && ls_row-key.
ENDLOOP.
ASSERT lv_keys = \`ABC\`.
READ TABLE lt_rows INTO ls_row WITH KEY amount = -7.
ASSERT sy-subrc = 0.
ASSERT ls_row-key = 'B'.`);
  });

});
