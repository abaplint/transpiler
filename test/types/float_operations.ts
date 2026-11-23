import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";
import {expect} from "chai";

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
    {filename: "cx_sy_conversion_no_number.clas.abap", contents: exceptionClass("cx_sy_conversion_no_number", "cx_root")},
    {filename: "cx_sy_arg_out_of_domain.clas.abap", contents: exceptionClass("cx_sy_arg_out_of_domain", "cx_root")},
    {filename: "cx_sy_arithmetic_overflow.clas.abap", contents: exceptionClass("cx_sy_arithmetic_overflow", "cx_root")}]);
  abap.Classes["CX_ROOT"] = class CxRoot {};
  abap.Classes["CX_SY_ZERODIVIDE"] = class CxSyZerodivide extends abap.Classes["CX_ROOT"] {};
  abap.Classes["CX_SY_CONVERSION_NO_NUMBER"] = class CxSyConversionNoNumber extends abap.Classes["CX_ROOT"] {};
  abap.Classes["CX_SY_ARG_OUT_OF_DOMAIN"] = class CxSyArgOutOfDomain extends abap.Classes["CX_ROOT"] {};
  abap.Classes["CX_SY_ARITHMETIC_OVERFLOW"] = class CxSyArithmeticOverflow extends abap.Classes["CX_ROOT"] {};
  const f = new AsyncFunction("abap", js);
  await f(abap);
}

// Operations on f, each snippet run on a system (ABAP 816) with the same ASSERTs; the values
// are compared, not written, as string templates write an f differently (#2185)
describe("Running Examples - f operations", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("sqrt, exp, log, log10 and the trigonometric and hyperbolic functions", async () => {
    await run(`
DATA lv_a TYPE f.
DATA lv_r TYPE f.
lv_a = 2.
lv_r = sqrt( lv_a ).
ASSERT lv_r = '1.4142135623730951'.
lv_r = exp( lv_a ).
ASSERT lv_r = '7.38905609893065'.
lv_r = log( lv_a ).
ASSERT lv_r = '0.6931471805599453'.
lv_r = log10( 1000 ).
ASSERT lv_r = 3.
lv_a = 1.
lv_r = sin( lv_a ).
ASSERT lv_r = '0.8414709848078965'.
lv_r = cos( lv_a ).
ASSERT lv_r = '0.5403023058681398'.
lv_r = tan( lv_a ).
ASSERT lv_r = '1.5574077246549023'.
lv_r = asin( lv_a ).
ASSERT lv_r = '1.5707963267948966'.
lv_r = acos( '0.5' ).
ASSERT lv_r = '1.0471975511965979'.
lv_r = atan( lv_a ).
ASSERT lv_r = '0.7853981633974483'.
lv_r = sinh( lv_a ).
ASSERT lv_r = '1.1752011936438014'.
lv_r = cosh( lv_a ).
ASSERT lv_r = '1.5430806348152437'.
lv_r = tanh( lv_a ).
ASSERT lv_r = '0.7615941559557649'.`);
  });

  it("sqrt, log, log10, acos outside their domain, log( 0 ), #2190", async () => {
    await run(`
DATA lv_r TYPE f.
DATA lv_result TYPE string.
TRY.
    lv_r = sqrt( -1 ).
    lv_result = lv_result && 'V'.
  CATCH cx_sy_arg_out_of_domain.
    lv_result = lv_result && 'D'.
  CATCH cx_sy_arithmetic_overflow.
    lv_result = lv_result && 'O'.
ENDTRY.
TRY.
    lv_r = log( 0 ).
    lv_result = lv_result && 'V'.
  CATCH cx_sy_arg_out_of_domain.
    lv_result = lv_result && 'D'.
  CATCH cx_sy_arithmetic_overflow.
    lv_result = lv_result && 'O'.
ENDTRY.
TRY.
    lv_r = log( -1 ).
    lv_result = lv_result && 'V'.
  CATCH cx_sy_arg_out_of_domain.
    lv_result = lv_result && 'D'.
  CATCH cx_sy_arithmetic_overflow.
    lv_result = lv_result && 'O'.
ENDTRY.
TRY.
    lv_r = log10( -1 ).
    lv_result = lv_result && 'V'.
  CATCH cx_sy_arg_out_of_domain.
    lv_result = lv_result && 'D'.
  CATCH cx_sy_arithmetic_overflow.
    lv_result = lv_result && 'O'.
ENDTRY.
TRY.
    lv_r = acos( 2 ).
    lv_result = lv_result && 'V'.
  CATCH cx_sy_arg_out_of_domain.
    lv_result = lv_result && 'D'.
  CATCH cx_sy_arithmetic_overflow.
    lv_result = lv_result && 'O'.
ENDTRY.
" D = CX_SY_ARG_OUT_OF_DOMAIN, O = CX_SY_ARITHMETIC_OVERFLOW, V = a value
ASSERT lv_result = 'DODDD'.`);
  });

  it("** with a fractional, whole and negative exponent, and ipow( )", async () => {
    await run(`
DATA lv_a TYPE f.
DATA lv_r TYPE f.
lv_a = 2.
lv_r = lv_a ** '0.5'.
ASSERT lv_r = '1.4142135623730951'.
lv_r = lv_a ** 10.
ASSERT lv_r = 1024.
lv_r = ipow( base = lv_a exp = 10 ).
ASSERT lv_r = 1024.
lv_r = lv_a ** -1.
ASSERT lv_r = '0.5'.`);
  });

  it("abs, sign, ceil, floor, trunc, frac, nmax and nmin; 0.1 + 0.2 is not 0.3", async () => {
    await run(`
DATA lv_a TYPE f.
DATA lv_b TYPE f.
DATA lv_r TYPE f.
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
lv_b = 3.
lv_r = nmax( val1 = lv_a val2 = lv_b ).
ASSERT lv_r = 3.
lv_r = nmin( val1 = lv_a val2 = lv_b ).
ASSERT lv_r = '-2.5'.
lv_a = '0.1'.
lv_b = '0.2'.
lv_r = lv_a + lv_b.
ASSERT lv_r <> '0.3'.
ASSERT lv_r = '0.30000000000000004'.`);
  });

  it("to i rounded half away from zero, to int8, decfloat34 and p, from text and from p", async () => {
    await run(`
DATA lv_a TYPE f.
DATA lv_i TYPE i.
DATA lv_i8 TYPE int8.
DATA lv_d TYPE decfloat34.
DATA lv_p TYPE p LENGTH 8 DECIMALS 2.
lv_a = '2.5'.
lv_i = lv_a.
ASSERT lv_i = 3.
lv_a = '-2.5'.
lv_i = lv_a.
ASSERT lv_i = -3.
lv_a = '2.4999'.
lv_i = lv_a.
ASSERT lv_i = 2.
lv_a = '1E15'.
lv_i8 = lv_a.
ASSERT lv_i8 = 1000000000000000.
lv_a = '1.5'.
lv_d = lv_a.
ASSERT lv_d = '1.5'.
lv_a = '1.125'.
lv_p = lv_a.
ASSERT lv_p = '1.13'.
lv_a = '1E3'.
ASSERT lv_a = 1000.
lv_a = ' 12 '.
ASSERT lv_a = 12.
lv_a = '1.5E-3'.
ASSERT lv_a = '0.0015'.
lv_p = '-3.25'.
lv_a = lv_p.
ASSERT lv_a = '-3.25'.`);
  });

  it("comparisons with f, i, p, decfloat34, c and string", async () => {
    await run(`
DATA lv_a TYPE f.
DATA lv_b TYPE f.
DATA lv_i TYPE i VALUE 2.
DATA lv_p TYPE p LENGTH 8 DECIMALS 2 VALUE '1.25'.
DATA lv_d TYPE decfloat34 VALUE '1.3'.
DATA lv_c TYPE c LENGTH 5 VALUE '1.250'.
DATA lv_s TYPE string VALUE \`-3\`.
lv_a = '1.25'.
lv_b = '1.3'.
ASSERT lv_a < lv_b.
ASSERT lv_b > lv_a.
ASSERT lv_a <= '1.25'.
ASSERT lv_a >= 1.
ASSERT lv_a BETWEEN 1 AND lv_b.
ASSERT lv_a < lv_i.
ASSERT lv_a = lv_p.
ASSERT lv_a < lv_d.
ASSERT lv_a = lv_c.
ASSERT lv_a > lv_s.
ASSERT NOT lv_a > lv_b.
lv_a = '-0.01'.
ASSERT lv_a < 0.`);
  });

  it("to c, decimal point and right-aligned, #2186", async () => {
    await run(`
DATA lv_f TYPE f.
DATA lv_c TYPE c LENGTH 24.
DATA lv_s TYPE string.
lv_f = '-1.5'.
lv_c = lv_f.
lv_s = |[{ lv_c }]|.
ASSERT lv_s = \`[ -1.5000000000000000E+00]\`.
lv_f = '1234.5'.
lv_c = lv_f.
lv_s = |[{ lv_c }]|.
ASSERT lv_s = \`[  1.2345000000000000E+03]\`.`);
  });

  it("ADD, SUBTRACT, MULTIPLY, DIVIDE and +=, -=, *=, /=", async () => {
    await run(`
DATA lv_r TYPE f.
lv_r = 10.
ADD '2.5' TO lv_r.
ASSERT lv_r = '12.5'.
SUBTRACT '0.75' FROM lv_r.
ASSERT lv_r = '11.75'.
MULTIPLY lv_r BY 2.
ASSERT lv_r = '23.5'.
DIVIDE lv_r BY 4.
ASSERT lv_r = '5.875'.
lv_r += '0.5'.
ASSERT lv_r = '6.375'.
lv_r -= 1.
ASSERT lv_r = '5.375'.
lv_r *= 4.
ASSERT lv_r = '21.5'.
lv_r /= 8.
ASSERT lv_r = '2.6875'.`);
  });

  it("division by zero is CX_SY_ZERODIVIDE and keeps the target, 0 / 0 is 0, text that is no number", async () => {
    await run(`
DATA lv_a TYPE f.
DATA lv_b TYPE f.
DATA lv_r TYPE f.
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
ASSERT lv_r = 0.
CLEAR lv_caught.
TRY.
    lv_a = 'abc'.
  CATCH cx_sy_conversion_no_number.
    lv_caught = abap_true.
ENDTRY.
ASSERT lv_caught = abap_true.`);
  });

  it("string templates: DECIMALS, STYLE = SCIENTIFIC and whole numbers", async () => {
    await run(`
DATA lv_a TYPE f.
lv_a = '-1234.5'.
ASSERT |{ lv_a DECIMALS = 2 }| = \`-1234.50\`.
ASSERT |{ lv_a STYLE = SCIENTIFIC }| = \`-1.2345E+03\`.
lv_a = 2.
ASSERT |{ lv_a }| = \`2\`.
lv_a = '1E20'.
ASSERT |{ lv_a }| = \`100000000000000000000\`.`);
  });

  it("string templates: 17 significant digits without trailing zeros, #2185", async () => {
    await run(`
DATA lv_f TYPE f.
lv_f = '0.25'.
ASSERT |{ lv_f }| = \`0.25\`.
lv_f = '0.1'.
ASSERT |{ lv_f }| = \`0.10000000000000001\`.
lv_f = 1 / 3.
ASSERT |{ lv_f }| = \`0.33333333333333331\`.
lv_f = '-1234.5'.
ASSERT |{ lv_f }| = \`-1234.5\`.
ASSERT |{ lv_f NUMBER = RAW }| = \`-1234.5\`.
ASSERT |{ lv_f STYLE = SIMPLE }| = \`-1234.5\`.
lv_f = '1E-10'.
ASSERT |{ lv_f }| = \`0.0000000001\`.`);
  });

  it("string templates: STYLE = ENGINEERING, #2185", async () => {
    await run(`
DATA lv_f TYPE f.
lv_f = '-1234.5'.
ASSERT |{ lv_f STYLE = ENGINEERING }| = \`-1.2345E+03\`.
lv_f = '0.25'.
ASSERT |{ lv_f STYLE = ENGINEERING }| = \`250E-03\`.
lv_f = '0.1'.
ASSERT |{ lv_f STYLE = ENGINEERING }| = \`100.00000000000001E-03\`.
lv_f = '0.001234'.
ASSERT |{ lv_f STYLE = ENGINEERING }| = \`1.2340000000000001E-03\`.
lv_f = 0.
ASSERT |{ lv_f STYLE = ENGINEERING }| = \`0E+00\`.
lv_f = '12.5'.
ASSERT |{ lv_f STYLE = ENGINEERING }| = \`12.5E+00\`.
lv_f = '1E20'.
ASSERT |{ lv_f STYLE = ENGINEERING }| = \`100E+18\`.
lv_f = '1E100'.
ASSERT |{ lv_f STYLE = ENGINEERING }| = \`10E+99\`.
lv_f = '1.5E-200'.
ASSERT |{ lv_f STYLE = ENGINEERING }| = \`15E-201\`.`);
  });

  it("string templates: STYLE together with DECIMALS, WIDTH and EXPONENT, #2185", async () => {
    // abaplint does not parse STYLE together with other formatting options yet, so the runtime is called directly
    const format = (value: number, options: object) => {
      const f = new abap.types.Float();
      f.set(value);
      return abap.templateFormatting(f as any, options as any);
    };
    expect(format(-1234.5, {style: "engineering", decimals: 2})).to.equal("-1.23E+03");
    expect(format(0.25, {style: "engineering", decimals: 2})).to.equal("250.00E-03");
    expect(format(0, {style: "engineering", decimals: 2})).to.equal("0.00E+00");
    expect(format(999.995, {style: "engineering", decimals: 2})).to.equal("1.00E+03");
    expect(format(999.995, {style: "engineering", decimals: 0})).to.equal("1E+03");
    expect(format(123456789012345678, {style: "engineering", decimals: 2})).to.equal("123.46E+15");
    expect(format(-1234.5, {style: "engineering", width: 15, align: "right"})).to.equal("    -1.2345E+03");
    expect(format(-1234.5, {style: "scientific", exponent: 2})).to.equal("-12.345E+02");
    expect(format(1E15, {style: "scientific", exponent: 0})).to.equal("1000000000000000E+00");
    expect(format(1E15, {style: "scientific", decimals: 2})).to.equal("1.00E+15");
    expect(format(999.995, {style: "scientific", decimals: 2})).to.equal("1.000E+03");
  });

  it("string templates: EXPONENT, #2185", async () => {
    await run(`
DATA lv_f TYPE f.
DATA lv_e TYPE i VALUE 3.
lv_f = '-1234.5'.
ASSERT |{ lv_f EXPONENT = 2 }| = \`-12.345E+02\`.
ASSERT |{ lv_f EXPONENT = 0 }| = \`-1234.5\`.
ASSERT |{ lv_f EXPONENT = -2 }| = \`-123450E-02\`.
ASSERT |{ lv_f EXPONENT = 5 }| = \`-0.012345E+05\`.
ASSERT |{ lv_f NUMBER = RAW EXPONENT = 2 }| = \`-12.345E+02\`.
ASSERT |{ lv_f EXPONENT = 2 DECIMALS = 2 }| = \`-12.35E+02\`.
ASSERT |{ lv_f EXPONENT = 2 SIGN = RIGHT }| = \`12.345E+02-\`.
ASSERT |{ lv_f EXPONENT = lv_e }| = \`-1.2345E+03\`.
lv_f = '0.1'.
ASSERT |{ lv_f EXPONENT = 2 }| = \`0.0010000000000000001E+02\`.
ASSERT |{ lv_f EXPONENT = 2 DECIMALS = 2 }| = \`0.00E+02\`.
lv_f = 0.
ASSERT |{ lv_f EXPONENT = 2 }| = \`0E+02\`.
ASSERT |{ lv_f EXPONENT = 0 }| = \`0\`.
lv_f = 2.
ASSERT |{ lv_f EXPONENT = -2 }| = \`200E-02\`.
lv_f = '12.5'.
ASSERT |{ lv_f EXPONENT = 2 DECIMALS = 2 }| = \`0.13E+02\`.
lv_f = '1E15'.
ASSERT |{ lv_f EXPONENT = 2 DECIMALS = 2 }| = \`10000000000000.00E+02\`.
lv_f = '1E17'.
ASSERT |{ lv_f EXPONENT = 2 DECIMALS = 2 }| = \`1000000000000000.0E+02\`.
lv_f = '1E20'.
ASSERT |{ lv_f EXPONENT = 2 }| = \`1000000000000000000E+02\`.
ASSERT |{ lv_f EXPONENT = 2 DECIMALS = 2 }| = \`1000000000000000000E+02\`.
lv_f = '-0.001'.
ASSERT |{ lv_f EXPONENT = 2 DECIMALS = 2 }| = \`-0.00E+02\`.`);
  });

  it("string templates: STYLE = SCIENTIFIC with 17 significant digits, #2185", async () => {
    await run(`
DATA lv_f TYPE f.
lv_f = '0.1'.
ASSERT |{ lv_f STYLE = SCIENTIFIC }| = \`1.0000000000000001E-01\`.
lv_f = 1 / 3.
ASSERT |{ lv_f STYLE = SCIENTIFIC }| = \`3.3333333333333331E-01\`.
lv_f = '999.9'.
ASSERT |{ lv_f STYLE = SCIENTIFIC }| = \`9.9989999999999998E+02\`.
lv_f = '-1234.5'.
ASSERT |{ lv_f STYLE = SCIENTIFIC }| = \`-1.2345E+03\`.
lv_f = '1.5E-200'.
ASSERT |{ lv_f STYLE = SCIENTIFIC }| = \`1.5E-200\`.`);
  });

  it("string templates: DECIMALS, an f has at most 17 digits", async () => {
    await run(`
DATA lv_f TYPE f.
lv_f = '1E15'.
ASSERT |{ lv_f DECIMALS = 2 }| = \`1000000000000000.0\`.
lv_f = '1E16'.
ASSERT |{ lv_f DECIMALS = 2 }| = \`10000000000000000\`.
lv_f = '123456789012345678'.
ASSERT |{ lv_f DECIMALS = 2 }| = \`123456789012345680\`.
lv_f = '999.995'.
ASSERT |{ lv_f DECIMALS = 2 }| = \`1000.00\`.
ASSERT |{ lv_f EXPONENT = 0 DECIMALS = 2 }| = \`1000.00\`.
lv_f = '-0.001'.
ASSERT |{ lv_f DECIMALS = 2 }| = \`-0.00\`.
lv_f = '1E100'.
ASSERT |{ lv_f }| = \`1${"0".repeat(100)}\`.`);
  });

  it("string templates: STYLE = ENGINEERING for i and p", async () => {
    await run(`
DATA lv_i TYPE i VALUE -1234.
DATA lv_p TYPE p LENGTH 8 DECIMALS 2 VALUE '-1234.5'.
ASSERT |{ lv_i STYLE = ENGINEERING }| = \`-1.234E+03\`.
ASSERT |{ lv_p STYLE = ENGINEERING }| = \`-1.2345E+03\`.`);
  });

  it("COLLECT sums, SORT and READ TABLE WITH KEY on an f component", async () => {
    await run(`
TYPES: BEGIN OF ty_row,
         key    TYPE c LENGTH 1,
         amount TYPE f,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls_row TYPE ty_row.
DATA lv_keys TYPE string.
ls_row-key = 'B'.
ls_row-amount = '-1.25'.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'A'.
ls_row-amount = '2.25'.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'B'.
ls_row-amount = '0.5'.
COLLECT ls_row INTO lt_rows.
ls_row-key = 'C'.
ls_row-amount = -3.
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
ASSERT ls_row-key = 'B'.`);
  });

});
