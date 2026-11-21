import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

const cxroot = `
CLASS cx_root DEFINITION PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.`;

const cxzerodivide = `
CLASS cx_sy_zerodivide DEFINITION PUBLIC INHERITING FROM cx_root.
ENDCLASS.
CLASS cx_sy_zerodivide IMPLEMENTATION.
ENDCLASS.`;

async function run(contents: string) {
  // the program first: runFiles evaluates the first object only, so the
  // exception classes are in the registry for the syntax check and stand in
  // as plain JavaScript classes at run time
  const js = await runFiles(abap, [
    {filename: "zfoobar.prog.abap", contents},
    {filename: "cx_root.clas.abap", contents: cxroot},
    {filename: "cx_sy_zerodivide.clas.abap", contents: cxzerodivide}]);
  abap.Classes["CX_ROOT"] = class CxRoot {};
  abap.Classes["CX_SY_ZERODIVIDE"] = class CxSyZerodivide extends abap.Classes["CX_ROOT"] {};
  const f = new AsyncFunction("abap", js);
  await f(abap);
}

// Operations on packed numbers, each snippet run on a system (ABAP 816) with the same ASSERTs
describe("Running Examples - packed operations", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("DIV, the quotient rounds down for a positive divisor and up for a negative one", async () => {
    await run(`
DATA lv_a TYPE p LENGTH 8 DECIMALS 2.
DATA lv_b TYPE p LENGTH 8 DECIMALS 2.
DATA lv_r TYPE p LENGTH 8 DECIMALS 2.
lv_a = '7.5'.
lv_b = 2.
lv_r = lv_a DIV lv_b.
ASSERT lv_r = 3.
lv_a = '-7.5'.
lv_r = lv_a DIV lv_b.
ASSERT lv_r = -4.
lv_b = -2.
lv_r = lv_a DIV lv_b.
ASSERT lv_r = 4.
lv_a = '7.5'.
lv_r = lv_a DIV lv_b.
ASSERT lv_r = -3.`);
  });

  it("** with a p base", async () => {
    await run(`
DATA lv_a TYPE p LENGTH 8 DECIMALS 2.
DATA lv_r TYPE p LENGTH 8 DECIMALS 2.
DATA lv_r3 TYPE p LENGTH 8 DECIMALS 3.
lv_a = '1.5'.
lv_r = lv_a ** 2.
ASSERT lv_r = '2.25'.
lv_r3 = lv_a ** 3.
ASSERT lv_r3 = '3.375'.`);
  });

  it("to i and to fewer decimals, rounded half away from zero", async () => {
    await run(`
DATA lv_a TYPE p LENGTH 8 DECIMALS 2.
DATA lv_r TYPE p LENGTH 8 DECIMALS 2.
DATA lv_r3 TYPE p LENGTH 8 DECIMALS 3.
DATA lv_i TYPE i.
lv_a = '2.5'.
lv_i = lv_a.
ASSERT lv_i = 3.
lv_a = '-2.5'.
lv_i = lv_a.
ASSERT lv_i = -3.
lv_a = '2.49'.
lv_i = lv_a.
ASSERT lv_i = 2.
lv_r3 = '1.005'.
lv_r = lv_r3.
ASSERT lv_r = '1.01'.
lv_r3 = '-1.005'.
lv_r = lv_r3.
ASSERT lv_r = '-1.01'.
lv_r3 = '-1.004'.
lv_r = lv_r3.
ASSERT lv_r = -1.`);
  });

  it("comparisons with p, i, f, c and string", async () => {
    await run(`
DATA lv_a TYPE p LENGTH 8 DECIMALS 2.
DATA lv_b TYPE p LENGTH 8 DECIMALS 1.
DATA lv_i TYPE i VALUE 2.
DATA lv_f TYPE f.
DATA lv_c TYPE c LENGTH 5 VALUE '1.250'.
DATA lv_s TYPE string VALUE \`-3\`.
lv_a = '1.25'.
lv_b = '1.3'.
lv_f = '1.2'.
ASSERT lv_a < lv_b.
ASSERT lv_b > lv_a.
ASSERT lv_a <= '1.25'.
ASSERT lv_a >= 1.
ASSERT lv_a BETWEEN 1 AND lv_b.
ASSERT lv_a < lv_i.
ASSERT lv_a > lv_f.
ASSERT lv_a = lv_c.
ASSERT lv_a > lv_s.
ASSERT NOT lv_a > lv_b.
lv_a = '-0.01'.
ASSERT lv_a < 0.`);
  });

  it("ADD, SUBTRACT, MULTIPLY, DIVIDE and +=, -=, *=, /=, rounded after each step", async () => {
    await run(`
DATA lv_r TYPE p LENGTH 8 DECIMALS 2.
lv_r = '10.00'.
ADD '2.5' TO lv_r.
ASSERT lv_r = '12.50'.
SUBTRACT '0.75' FROM lv_r.
ASSERT lv_r = '11.75'.
MULTIPLY lv_r BY '1.5'.
ASSERT lv_r = '17.63'.
DIVIDE lv_r BY 4.
ASSERT lv_r = '4.41'.
lv_r += '0.5'.
ASSERT lv_r = '4.91'.
lv_r -= 1.
ASSERT lv_r = '3.91'.
lv_r *= 2.
ASSERT lv_r = '7.82'.
lv_r /= 3.
ASSERT lv_r = '2.61'.`);
  });

  it("division by zero is CX_SY_ZERODIVIDE and keeps the target, 0 / 0 is 0", async () => {
    await run(`
DATA lv_a TYPE p LENGTH 8 DECIMALS 2 VALUE '1.5'.
DATA lv_b TYPE p LENGTH 8 DECIMALS 1.
DATA lv_r TYPE p LENGTH 8 DECIMALS 2 VALUE 5.
DATA lv_caught TYPE abap_bool.
TRY.
    lv_r = lv_a / lv_b.
  CATCH cx_sy_zerodivide.
    lv_caught = abap_true.
ENDTRY.
ASSERT lv_caught = abap_true.
ASSERT lv_r = 5.
CLEAR lv_a.
lv_r = lv_a / lv_b.
ASSERT lv_r = 0.`);
  });

  it("COLLECT sums, SORT and READ TABLE WITH KEY on a p component", async () => {
    await run(`
TYPES: BEGIN OF ty_row,
         key    TYPE c LENGTH 1,
         amount TYPE p LENGTH 8 DECIMALS 2,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls_row TYPE ty_row.
DATA lv_keys TYPE string.
ls_row-key = 'B'.
ls_row-amount = '-1.10'.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'A'.
ls_row-amount = '2.25'.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'B'.
ls_row-amount = '0.35'.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'C'.
ls_row-amount = '-3.00'.
COLLECT ls_row INTO lt_rows.
ASSERT lines( lt_rows ) = 3.
READ TABLE lt_rows INTO ls_row WITH KEY key = 'B'.
ASSERT ls_row-amount = '-0.75'.
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
READ TABLE lt_rows INTO ls_row WITH KEY amount = '-0.75'.
ASSERT sy-subrc = 0.
ASSERT ls_row-key = 'B'.
READ TABLE lt_rows TRANSPORTING NO FIELDS WITH KEY amount = '-0.7'.
ASSERT sy-subrc = 4.`);
  });

});
