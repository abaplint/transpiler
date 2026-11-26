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

  it("a timestamp with seven decimals is copied exactly", async () => {
    await run(`
DATA lv_first TYPE p LENGTH 11 DECIMALS 7.
DATA lv_second TYPE p LENGTH 11 DECIMALS 7.
lv_first = '20240229235959'.
lv_second = lv_first.
ASSERT lv_second = '20240229235959.0000000'.
ASSERT |{ lv_second }| = '20240229235959.0000000'.
lv_first = '20240229235959.1234567'.
lv_second = lv_first.
ASSERT |{ lv_second }| = '20240229235959.1234567'.`);
  });

  it("a short timestamp assigned to a long one keeps the whole minute", async () => {
    await run(`
DATA lv_short TYPE p LENGTH 8 DECIMALS 0.
DATA lv_long TYPE p LENGTH 11 DECIMALS 7.
lv_short = 20261014103400.
lv_long = lv_short.
ASSERT |{ lv_long }| = '20261014103400.0000000'.`);
  });

  it("p plus and minus p, exact beyond the digits of a double", async () => {
    await run(`
DATA lv_long TYPE p LENGTH 11 DECIMALS 7.
DATA lv_step TYPE p LENGTH 11 DECIMALS 7.
DATA lv_result TYPE p LENGTH 11 DECIMALS 7.
lv_long = '20240229235959.1234567'.
lv_step = '0.0000001'.
lv_result = lv_long + lv_step.
ASSERT |{ lv_result }| = '20240229235959.1234568'.
lv_result = lv_long - lv_step.
ASSERT |{ lv_result }| = '20240229235959.1234566'.
lv_result = lv_long - lv_long.
ASSERT lv_result = 0.`);
  });

  it("p with i and int8 operands, exact above 2 ** 53", async () => {
    await run(`
DATA lv_big TYPE p LENGTH 16 DECIMALS 0.
DATA lv_result TYPE p LENGTH 16 DECIMALS 0.
DATA lv_int TYPE i VALUE 1.
DATA lv_int8 TYPE int8 VALUE 2.
lv_big = '9007199254740993'.
lv_result = lv_big + lv_int.
ASSERT |{ lv_result }| = '9007199254740994'.
lv_result = lv_int + lv_big.
ASSERT |{ lv_result }| = '9007199254740994'.
lv_result = lv_big - lv_int8.
ASSERT |{ lv_result }| = '9007199254740991'.
lv_result = lv_big * lv_int.
ASSERT |{ lv_result }| = '9007199254740993'.
lv_result = lv_big + 1.
ASSERT |{ lv_result }| = '9007199254740994'.`);
  });

  it("p times p, exact, rounded half away from zero into the target", async () => {
    await run(`
DATA lv_left TYPE p LENGTH 16 DECIMALS 2.
DATA lv_right TYPE p LENGTH 8 DECIMALS 2.
DATA lv_result TYPE p LENGTH 16 DECIMALS 2.
lv_left = '12345678901234567.89'.
lv_right = '3.00'.
lv_result = lv_left * lv_right.
ASSERT |{ lv_result }| = '37037036703703703.67'.
lv_left = '0.05'.
lv_right = '0.50'.
lv_result = lv_left * lv_right.
ASSERT |{ lv_result }| = '0.03'.
lv_left = '-0.05'.
lv_result = lv_left * lv_right.
ASSERT |{ lv_result }| = '-0.03'.`);
  });

  it("p operands with different decimals", async () => {
    await run(`
DATA lv_two TYPE p LENGTH 8 DECIMALS 2.
DATA lv_five TYPE p LENGTH 8 DECIMALS 5.
DATA lv_result TYPE p LENGTH 8 DECIMALS 5.
DATA lv_int TYPE i.
lv_two = '1.25'.
lv_five = '0.00001'.
lv_result = lv_two + lv_five.
ASSERT |{ lv_result }| = '1.25001'.
lv_result = lv_two - lv_five.
ASSERT |{ lv_result }| = '1.24999'.
lv_int = lv_two + lv_two.
ASSERT lv_int = 3.`);
  });

  it("p through field symbols, exact for all three operators", async () => {
    await run(`
DATA lv_big TYPE p LENGTH 16 DECIMALS 0.
DATA lv_one TYPE p LENGTH 16 DECIMALS 0.
DATA lv_result TYPE p LENGTH 16 DECIMALS 0.
FIELD-SYMBOLS <lv_big> TYPE any.
FIELD-SYMBOLS <lv_one> TYPE any.
lv_big = '9007199254740993'.
lv_one = 1.
ASSIGN lv_big TO <lv_big>.
ASSIGN lv_one TO <lv_one>.
lv_result = <lv_big> * lv_one.
ASSERT |{ lv_result }| = '9007199254740993'.
lv_result = <lv_big> * <lv_one>.
ASSERT |{ lv_result }| = '9007199254740993'.
lv_result = <lv_big> + <lv_one>.
ASSERT |{ lv_result }| = '9007199254740994'.
lv_result = <lv_big> - <lv_one>.
ASSERT |{ lv_result }| = '9007199254740992'.`);
  });

  it("MOD with a p operand keeps the decimals, also of a calculated p", async () => {
    await run(`
DATA lv_p TYPE p LENGTH 8 DECIMALS 2.
DATA lv_result TYPE p LENGTH 8 DECIMALS 2.
lv_p = '2.50'.
lv_result = lv_p MOD 2.
ASSERT lv_result = '0.50'.
lv_p = '1.25'.
lv_result = ( lv_p + lv_p ) MOD 2.
ASSERT lv_result = '0.50'.
lv_result = ( lv_p * 3 ) MOD 1.
ASSERT lv_result = '0.75'.
lv_result = ( lv_p - 3 ) MOD 2.
ASSERT lv_result = '0.25'.
lv_result = ( lv_p + lv_p ) DIV 2.
ASSERT lv_result = 1.`);
  });

  it("p into t and d is rounded, also a calculated p", async () => {
    await run(`
DATA lv_p TYPE p LENGTH 8 DECIMALS 2.
DATA lv_time TYPE t.
DATA lv_date TYPE d.
lv_p = '3.50'.
lv_time = lv_p.
ASSERT lv_time = '000004'.
lv_date = lv_p.
ASSERT lv_date = '00010105'.
lv_p = '1.75'.
lv_time = lv_p + lv_p.
ASSERT lv_time = '000004'.
lv_date = lv_p * 2.
ASSERT lv_date = '00010105'.
lv_time = lv_p * 2 + 1.
ASSERT lv_time = '000005'.`);
  });

  it("a calculated p into i, string and a string template", async () => {
    await run(`
DATA lv_p TYPE p LENGTH 8 DECIMALS 2.
DATA lv_int TYPE i.
DATA lv_string TYPE string.
DATA lv_q TYPE p LENGTH 8 DECIMALS 2.
lv_p = '1.25'.
lv_q = '1.26'.
lv_int = lv_p + lv_p.
ASSERT lv_int = 3.
lv_string = lv_p + lv_q.
ASSERT lv_string = \`2.51 \`.
ASSERT |{ lv_p + lv_q }| = '2.51'.
ASSERT |{ lv_p * lv_p }| = '1.5625'.
lv_p = '-1.25'.
lv_int = lv_p + lv_p.
ASSERT lv_int = -3.
lv_string = lv_p - 1.
ASSERT lv_string = \`2.25-\`.`);
  });

  it("MOD with p operands is exact and stays a p", async () => {
    await run(`
DATA lv_long TYPE p LENGTH 11 DECIMALS 7.
DATA lv_result TYPE p LENGTH 11 DECIMALS 7.
DATA lv_whole TYPE p LENGTH 8 DECIMALS 0.
DATA lv_small TYPE p LENGTH 8 DECIMALS 2.
DATA lv_string TYPE string.
lv_long = '20261003101500.3500000'.
lv_result = lv_long MOD 100.
ASSERT |{ lv_result }| = '0.3500000'.
lv_whole = 7.
lv_string = lv_whole MOD 2.
ASSERT lv_string = \`1 \`.
lv_small = '-7.50'.
lv_small = lv_small MOD 2.
ASSERT lv_small = '0.50'.
lv_small = '7.50'.
lv_small = lv_small MOD -2.
ASSERT lv_small = '1.50'.
lv_small = 0.
lv_small = lv_small MOD 0.
ASSERT lv_small = 0.`);
  });

  it("a field symbol on a p next to an int8", async () => {
    await run(`
DATA lv_p TYPE p LENGTH 8 DECIMALS 2 VALUE '1.25'.
DATA lv_int8 TYPE int8 VALUE 3.
DATA lv_result TYPE p LENGTH 8 DECIMALS 2.
FIELD-SYMBOLS <lv_p> TYPE any.
ASSIGN lv_p TO <lv_p>.
lv_result = <lv_p> + lv_int8.
ASSERT lv_result = '4.25'.
lv_result = lv_int8 - <lv_p>.
ASSERT lv_result = '1.75'.
lv_result = <lv_p> * lv_int8.
ASSERT lv_result = '3.75'.
lv_result = <lv_p> MOD lv_int8.
ASSERT lv_result = '1.25'.`);
  });

  it("p into xstring is rounded, also a calculated p", async () => {
    await run(`
DATA lv_p TYPE p LENGTH 8 DECIMALS 2 VALUE '1.25'.
DATA lv_xstring TYPE xstring.
lv_xstring = lv_p + lv_p.
ASSERT lv_xstring = '03'.
lv_p = '2.50'.
lv_xstring = lv_p.
ASSERT lv_xstring = '03'.`);
  });

  it("an int8 calculated with a whole p compares exactly", async () => {
    await run(`
DATA lv_a TYPE int8.
DATA lv_b TYPE int8.
DATA lv_one TYPE p LENGTH 8 DECIMALS 0 VALUE 1.
lv_a = 9223372036854775807.
lv_b = 9223372036854775806.
ASSERT lv_a - lv_one = lv_b.
ASSERT lv_b + lv_one = lv_a.
ASSERT lv_b + lv_one > lv_b.
ASSERT lv_one = 1.`);
  });

});
