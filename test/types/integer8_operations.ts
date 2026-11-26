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

// Operations on int8, each snippet run on a system (ABAP 816) with the same ASSERTs
describe("Running Examples - int8 operations", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("+, -, *, /, DIV and MOD exact above 2^53, / rounds half away from zero", async () => {
    await run(`
DATA lv_a TYPE int8.
DATA lv_b TYPE int8 VALUE 2.
DATA lv_r TYPE int8.
lv_a = 9007199254740993.
lv_r = lv_a + lv_b.
ASSERT |{ lv_r }| = \`9007199254740995\`.
lv_r = lv_a - lv_b.
ASSERT |{ lv_r }| = \`9007199254740991\`.
lv_r = lv_a * 3.
ASSERT |{ lv_r }| = \`27021597764222979\`.
lv_r = lv_a / lv_b.
ASSERT |{ lv_r }| = \`4503599627370497\`.
lv_r = lv_a DIV lv_b.
ASSERT |{ lv_r }| = \`4503599627370496\`.
lv_r = lv_a MOD lv_b.
ASSERT lv_r = 1.
lv_a = -7.
lv_r = lv_a / lv_b.
ASSERT lv_r = -4.`);
  });

  it("sign, nmax and nmin above 2^53", async () => {
    await run(`
DATA lv_a TYPE int8.
DATA lv_b TYPE int8.
DATA lv_r TYPE int8.
lv_a = -9007199254740993.
lv_b = 9007199254740995.
lv_r = sign( lv_a ).
ASSERT lv_r = -1.
lv_r = nmax( val1 = lv_a val2 = lv_b ).
ASSERT |{ lv_r }| = \`9007199254740995\`.
lv_r = nmin( val1 = lv_a val2 = lv_b ).
ASSERT |{ lv_r }| = \`-9007199254740993\`.`);
  });

  it("comparisons above 2^53 with int8, i and c", async () => {
    await run(`
DATA lv_a TYPE int8.
DATA lv_b TYPE int8.
DATA lv_i TYPE i VALUE 5.
DATA lv_c TYPE c LENGTH 20 VALUE '9007199254740992'.
lv_a = 9007199254740993.
lv_b = 9007199254740992.
ASSERT lv_a > lv_b.
ASSERT lv_b < lv_a.
ASSERT lv_a <> lv_b.
ASSERT lv_a >= lv_b.
ASSERT lv_a BETWEEN lv_b AND 9007199254740994.
ASSERT lv_a > lv_i.
ASSERT lv_a > lv_c.
ASSERT NOT lv_a = lv_c.`);
  });

  it("ADD, SUBTRACT, MULTIPLY, DIVIDE and +=, -=, *=, /= above 2^53", async () => {
    await run(`
DATA lv_r TYPE int8.
lv_r = 9007199254740990.
ADD 3 TO lv_r.
ASSERT |{ lv_r }| = \`9007199254740993\`.
SUBTRACT 1 FROM lv_r.
ASSERT |{ lv_r }| = \`9007199254740992\`.
MULTIPLY lv_r BY 2.
ASSERT |{ lv_r }| = \`18014398509481984\`.
DIVIDE lv_r BY 4.
ASSERT |{ lv_r }| = \`4503599627370496\`.
lv_r += 1.
ASSERT |{ lv_r }| = \`4503599627370497\`.
lv_r -= 3.
ASSERT |{ lv_r }| = \`4503599627370494\`.
lv_r *= 2.
ASSERT |{ lv_r }| = \`9007199254740988\`.
lv_r /= 3.
ASSERT |{ lv_r }| = \`3002399751580329\`.`);
  });

  it("division by zero is CX_SY_ZERODIVIDE and keeps the target, 0 / 0 is 0, text that is no number, also 1E3", async () => {
    await run(`
DATA lv_a TYPE int8 VALUE 7.
DATA lv_b TYPE int8.
DATA lv_r TYPE int8 VALUE 5.
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

  it("from text: sign last and blanks above 2^53, plus sign, empty", async () => {
    await run(`
DATA lv_r TYPE int8.
lv_r = '9007199254740993-'.
ASSERT |{ lv_r }| = \`-9007199254740993\`.
lv_r = ' 9007199254740993 '.
ASSERT |{ lv_r }| = \`9007199254740993\`.
lv_r = '+7'.
ASSERT lv_r = 7.
lv_r = ''.
ASSERT lv_r = 0.`);
  });

  it("to c with the sign last, string, p, decfloat34 and f above 2^53", async () => {
    await run(`
DATA lv_a TYPE int8.
DATA lv_c TYPE c LENGTH 24.
DATA lv_s TYPE string.
DATA lv_p TYPE p LENGTH 16.
DATA lv_d TYPE decfloat34.
DATA lv_f TYPE f.
lv_a = -9007199254740993.
lv_c = lv_a.
ASSERT lv_c = '       9007199254740993-'.
lv_s = lv_a.
ASSERT lv_s = \`9007199254740993-\`.
lv_p = lv_a.
ASSERT |{ lv_p }| = \`-9007199254740993\`.
lv_d = lv_a.
ASSERT |{ lv_d }| = \`-9007199254740993\`.
lv_f = lv_a.
ASSERT |{ lv_f STYLE = SCIENTIFIC }| = \`-9.007199254740992E+15\`.`);
  });

  it("string templates: WIDTH, NUMBER = RAW, above 2^53", async () => {
    await run(`
DATA lv_a TYPE int8 VALUE -1234.
ASSERT |{ lv_a }| = \`-1234\`.
ASSERT |[{ lv_a WIDTH = 8 }]| = \`[-1234   ]\`.
ASSERT |{ lv_a NUMBER = RAW }| = \`-1234\`.
lv_a = -9007199254740993.
ASSERT |{ lv_a }| = \`-9007199254740993\`.`);
  });

  it("COLLECT sums, SORT and READ TABLE WITH KEY on an int8 component above 2^53", async () => {
    await run(`
TYPES: BEGIN OF ty_row,
         key    TYPE c LENGTH 1,
         amount TYPE int8,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls_row TYPE ty_row.
DATA lv_big TYPE int8.
DATA lv_keys TYPE string.
lv_big = 9007199254740993.
ls_row-key = 'B'.
ls_row-amount = lv_big.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'A'.
ls_row-amount = 22.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'B'.
ls_row-amount = 2.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'C'.
ls_row-amount = -30.
COLLECT ls_row INTO lt_rows.
ASSERT lines( lt_rows ) = 3.
READ TABLE lt_rows INTO ls_row WITH KEY key = 'B'.
ASSERT |{ ls_row-amount }| = \`9007199254740995\`.
SORT lt_rows BY amount.
LOOP AT lt_rows INTO ls_row.
  lv_keys = lv_keys && ls_row-key.
ENDLOOP.
ASSERT lv_keys = \`CAB\`.
CLEAR lv_keys.
SORT lt_rows BY amount DESCENDING.
LOOP AT lt_rows INTO ls_row.
  lv_keys = lv_keys && ls_row-key.
ENDLOOP.
ASSERT lv_keys = \`BAC\`.
lv_big = lv_big + 2.
READ TABLE lt_rows INTO ls_row WITH KEY amount = lv_big.
ASSERT sy-subrc = 0.
ASSERT ls_row-key = 'B'.`);
  });

});
