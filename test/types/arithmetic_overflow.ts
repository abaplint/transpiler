import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

const cxroot = `
CLASS cx_root DEFINITION PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.`;

const cxoverflow = `
CLASS cx_sy_arithmetic_overflow DEFINITION PUBLIC INHERITING FROM cx_root.
ENDCLASS.
CLASS cx_sy_arithmetic_overflow IMPLEMENTATION.
ENDCLASS.`;

async function run(contents: string) {
  // the program first: runFiles evaluates the first object only, so the
  // exception classes are in the registry for the syntax check and stand in
  // as plain JavaScript classes at run time
  const js = await runFiles(abap, [
    {filename: "zfoobar.prog.abap", contents},
    {filename: "cx_root.clas.abap", contents: cxroot},
    {filename: "cx_sy_arithmetic_overflow.clas.abap", contents: cxoverflow}]);
  abap.Classes["CX_ROOT"] = class CxRoot {};
  abap.Classes["CX_SY_ARITHMETIC_OVERFLOW"] = class CxSyArithmeticOverflow extends abap.Classes["CX_ROOT"] {};
  const f = new AsyncFunction("abap", js);
  await f(abap);
}

// checked on a system: in calculation type int8 a result outside -2^63 .. 2^63-1 raises
// CX_SY_ARITHMETIC_OVERFLOW, also for an intermediate result; with a p, f or decfloat34
// target the calculation type is that of the target and there is no int8 overflow
describe("Running Examples - int8 arithmetic overflow", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("int8 results outside the int8 range", async () => {
    await run(`
DATA lv_max TYPE int8.
DATA lv_min TYPE int8.
DATA lv_one TYPE int8 VALUE 1.
DATA lv_m1 TYPE int8 VALUE -1.
DATA lv_two TYPE int8 VALUE 2.
DATA lv_r TYPE int8.
DATA lv_i TYPE i.
DATA lv_s TYPE string.
DATA lv_res TYPE string.
lv_max = 9223372036854775807.
lv_min = -9223372036854775807 - 1.
DO 16 TIMES.
  CLEAR lv_r.
  TRY.
      CASE sy-index.
        WHEN 1. lv_r = lv_max + lv_one.
        WHEN 2. lv_r = lv_min - lv_one.
        WHEN 3. lv_r = lv_max * lv_two.
        WHEN 4. lv_r = lv_min DIV lv_m1.
        WHEN 5. lv_r = lv_min MOD lv_m1.
        WHEN 6. lv_r = - lv_min.
        WHEN 7. lv_r = abs( lv_min ).
        WHEN 8. lv_r = abs( lv_m1 ).
        WHEN 9. lv_r = lv_max + lv_one - lv_one.
        WHEN 10. lv_r = lv_max - lv_one + lv_one.
        WHEN 11. lv_r = lv_max + 1.
        WHEN 12. lv_i = lv_max + lv_one.
        WHEN 13. lv_s = |{ lv_max + lv_one }|.
        WHEN 14. lv_r = lv_max. ADD lv_one TO lv_r.
        WHEN 15. lv_r = lv_min / lv_m1.
        WHEN 16. lv_r = lv_max * lv_max.
      ENDCASE.
      lv_res = |{ lv_res }{ sy-index }:{ lv_r } |.
    CATCH cx_sy_arithmetic_overflow.
      lv_res = |{ lv_res }{ sy-index }:overflow |.
  ENDTRY.
ENDDO.
ASSERT lv_res = \`1:overflow 2:overflow 3:overflow 4:overflow 5:0 6:overflow 7:overflow 8:1 \`
  && \`9:overflow 10:9223372036854775807 11:overflow 12:overflow 13:overflow 14:overflow \`
  && \`15:overflow 16:overflow \`.`);
  });

  it("p, f and decfloat34 targets, no int8 overflow", async () => {
    await run(`
DATA lv_max TYPE int8.
DATA lv_one TYPE int8 VALUE 1.
DATA lv_two TYPE int8 VALUE 2.
DATA lv_seven TYPE int8 VALUE 7.
DATA lv_p TYPE p LENGTH 16.
DATA lv_p2 TYPE p LENGTH 16 DECIMALS 2.
DATA lv_f TYPE f.
DATA lv_d TYPE decfloat34.
lv_max = 9223372036854775807.
lv_p = lv_max + lv_one.
ASSERT |{ lv_p }| = \`9223372036854775808\`.
lv_p = lv_max * lv_two + 1.
ASSERT |{ lv_p }| = \`18446744073709551615\`.
lv_p = ( lv_max + lv_one ) - lv_two.
ASSERT |{ lv_p }| = \`9223372036854775806\`.
lv_p2 = lv_seven / lv_two.
ASSERT lv_p2 = '3.50'.
lv_p = lv_seven / lv_two.
ASSERT lv_p = 4.
lv_d = lv_max + lv_max.
ASSERT |{ lv_d }| = \`18446744073709551614\`.
lv_f = lv_max + lv_one.
ASSERT lv_f = 9223372036854775808.`);
  });

});
