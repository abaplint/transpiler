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

// Operations on decfloat16 and decfloat34, each snippet run on a system (ABAP 816) for both types with the same ASSERTs
describe("Running Examples - decfloat operations", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  for (const type of ["decfloat16", "decfloat34"]) {

    it(`${type}: DIV, the quotient rounds down for a positive divisor and up for a negative one`, async () => {
      await run(`
DATA lv_a TYPE ${type}.
DATA lv_b TYPE ${type}.
DATA lv_r TYPE ${type}.
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

    it(`${type}: ** with an integer exponent, and ipow( )`, async () => {
      await run(`
DATA lv_a TYPE ${type}.
DATA lv_r TYPE ${type}.
lv_a = '1.5'.
lv_r = lv_a ** 2.
ASSERT lv_r = '2.25'.
lv_a = 2.
lv_r = lv_a ** -1.
ASSERT lv_r = '0.5'.
lv_r = ipow( base = lv_a exp = 10 ).
ASSERT lv_r = 1024.`);
    });

    it(`${type}: abs, sign, ceil, floor, trunc, frac, round with dec and prec, nmax and nmin`, async () => {
      await run(`
DATA lv_a TYPE ${type}.
DATA lv_b TYPE ${type}.
DATA lv_r TYPE ${type}.
lv_a = '-2.5'.
lv_r = abs( lv_a ).
ASSERT lv_r = '2.5'.
lv_r = sign( lv_a ).
ASSERT lv_r = -1.
lv_r = ceil( lv_a ).
ASSERT lv_r = -2.
lv_r = floor( lv_a ).
ASSERT lv_r = -3.
lv_r = trunc( lv_a ).
ASSERT lv_r = -2.
lv_r = frac( lv_a ).
ASSERT lv_r = '-0.5'.
lv_a = '2.345'.
lv_r = round( val = lv_a dec = 2 ).
ASSERT lv_r = '2.35'.
lv_r = round( val = lv_a prec = 2 ).
ASSERT lv_r = '2.3'.
lv_a = '-2.345'.
lv_r = round( val = lv_a dec = 2 ).
ASSERT lv_r = '-2.35'.
lv_a = '1.5'.
lv_b = -3.
lv_r = nmax( val1 = lv_a val2 = lv_b ).
ASSERT lv_r = '1.5'.
lv_r = nmin( val1 = lv_a val2 = lv_b ).
ASSERT lv_r = -3.`);
    });

    it(`${type}: to i and p rounded half away from zero, from text and from p`, async () => {
      await run(`
DATA lv_a TYPE ${type}.
DATA lv_i TYPE i.
DATA lv_p TYPE p LENGTH 8 DECIMALS 2.
lv_a = '2.5'.
lv_i = lv_a.
ASSERT lv_i = 3.
lv_a = '-2.5'.
lv_i = lv_a.
ASSERT lv_i = -3.
lv_a = '1.005'.
lv_p = lv_a.
ASSERT lv_p = '1.01'.
lv_a = '-1.005'.
lv_p = lv_a.
ASSERT lv_p = '-1.01'.
lv_a = '1E3'.
ASSERT lv_a = 1000.
lv_a = ' 12 '.
ASSERT lv_a = 12.
lv_a = '0.000001'.
ASSERT |{ lv_a }| = \`0.000001\`.
lv_p = '-3.10'.
lv_a = lv_p.
ASSERT |{ lv_a }| = \`-3.1\`.`);
    });

    it(`${type}: comparisons with decfloat, i, p, f, c and string, the scale does not count`, async () => {
      await run(`
DATA lv_a TYPE ${type}.
DATA lv_b TYPE ${type}.
DATA lv_i TYPE i VALUE 2.
DATA lv_p TYPE p LENGTH 8 DECIMALS 2 VALUE '1.25'.
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
ASSERT lv_a = lv_p.
ASSERT lv_a > lv_f.
ASSERT lv_a = lv_c.
ASSERT lv_a > lv_s.
ASSERT NOT lv_a > lv_b.
lv_b = '1.2500'.
ASSERT lv_a = lv_b.
lv_a = '-0.01'.
ASSERT lv_a < 0.`);
    });

    it(`${type}: ADD, SUBTRACT, MULTIPLY, DIVIDE and +=, -=, *=, /=`, async () => {
      await run(`
DATA lv_r TYPE ${type}.
lv_r = 10.
ADD '2.5' TO lv_r.
ASSERT lv_r = '12.5'.
SUBTRACT '0.75' FROM lv_r.
ASSERT lv_r = '11.75'.
MULTIPLY lv_r BY '1.5'.
ASSERT lv_r = '17.625'.
DIVIDE lv_r BY 4.
ASSERT lv_r = '4.40625'.
lv_r += '0.5'.
ASSERT lv_r = '4.90625'.
lv_r -= 1.
ASSERT lv_r = '3.90625'.
lv_r *= 2.
ASSERT lv_r = '7.8125'.
lv_r /= 4.
ASSERT lv_r = '1.953125'.`);
    });

    it(`${type}: division by zero is CX_SY_ZERODIVIDE and keeps the target, 0 / 0 is 0`, async () => {
      await run(`
DATA lv_a TYPE ${type}.
DATA lv_b TYPE ${type}.
DATA lv_r TYPE ${type}.
DATA lv_caught TYPE abap_bool.
lv_a = '1.5'.
lv_r = 5.
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

    it(`${type}: string templates: DECIMALS, STYLE = SIMPLE, NUMBER = RAW, trailing zeros, large and small values`, async () => {
      await run(`
DATA lv_a TYPE ${type}.
lv_a = '-1234.5'.
ASSERT |{ lv_a DECIMALS = 2 }| = \`-1234.50\`.
ASSERT |{ lv_a STYLE = SIMPLE }| = \`-1234.5\`.
ASSERT |{ lv_a NUMBER = RAW }| = \`-1234.5\`.
lv_a = '1.50'.
ASSERT |{ lv_a }| = \`1.5\`.
lv_a = '1E20'.
ASSERT |{ lv_a }| = \`100000000000000000000\`.
lv_a = '1E-10'.
ASSERT |{ lv_a }| = \`0.0000000001\`.`);
    });

    it(`${type}: SORT and READ TABLE WITH KEY on a decfloat component`, async () => {
      await run(`
TYPES: BEGIN OF ty_row,
         key    TYPE c LENGTH 1,
         amount TYPE ${type},
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls_row TYPE ty_row.
DATA lv_keys TYPE string.
ls_row-key = 'B'.
ls_row-amount = '-0.75'.
APPEND ls_row TO lt_rows.
ls_row-key = 'A'.
ls_row-amount = '2.25'.
APPEND ls_row TO lt_rows.
ls_row-key = 'C'.
ls_row-amount = -3.
APPEND ls_row TO lt_rows.
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
  }

});
