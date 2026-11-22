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

  it("a quotient is rounded to 16 digits", async () => {
    const code = `
    DATA lv_a TYPE decfloat16.
    DATA lv_b TYPE decfloat16.
    DATA lv_r TYPE decfloat16.
    lv_a = 1.
    lv_b = 3.
    lv_r = lv_a / lv_b.
    WRITE / |{ lv_r }|.
    lv_r = lv_r * lv_b.
    WRITE / |{ lv_r }|.`;
    expect(await runAndGet(code)).to.equal("0.3333333333333333\n0.9999999999999999");
  });

  it("an expression is calculated in decfloat34, only the result is rounded", async () => {
    const code = `
    DATA lv_a TYPE decfloat16.
    DATA lv_b TYPE decfloat16.
    DATA lv_r TYPE decfloat16.
    DATA lv_34 TYPE decfloat34.
    lv_a = 1.
    lv_b = 3.
    lv_r = lv_a / lv_b * lv_b.
    WRITE / |{ lv_r }|.
    lv_34 = lv_a / lv_b.
    WRITE / |{ lv_34 }|.
    lv_r = 1.
    lv_r = lv_r + '0.00000000000000001'.
    WRITE / |{ lv_r }|.
    lv_r = 1.
    ADD '0.00000000000000001' TO lv_r.
    WRITE / |{ lv_r }|.`;
    expect(await runAndGet(code)).to.equal("1\n0.3333333333333333333333333333333333\n1\n1");
  });

  it("sqrt( ) into decfloat16", async () => {
    const code = `
    DATA lv_a TYPE decfloat16.
    DATA lv_r TYPE decfloat16.
    lv_a = 2.
    lv_r = sqrt( lv_a ).
    WRITE / |{ lv_r }|.`;
    expect(await runAndGet(code)).to.equal("1.414213562373095");
  });

  it("text into decfloat16 is rounded half away from zero", async () => {
    const code = `
    DATA lv TYPE decfloat16.
    DATA lt_in TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_in TYPE string.
    APPEND \`1234567890123456789\` TO lt_in.
    APPEND \`1.0000000000000005\` TO lt_in.
    APPEND \`1.0000000000000015\` TO lt_in.
    APPEND \`1.0000000000000025\` TO lt_in.
    APPEND \`-1.0000000000000025\` TO lt_in.
    APPEND \`1.00000000000000049\` TO lt_in.
    APPEND \`0.1234567890123456789E-10\` TO lt_in.
    APPEND \`9999999999999999.5\` TO lt_in.
    LOOP AT lt_in INTO lv_in.
      lv = lv_in.
      WRITE / |{ lv }|.
    ENDLOOP.`;
    expect(await runAndGet(code)).to.equal([
      "1234567890123457000",
      "1.000000000000001",
      "1.000000000000002",
      "1.000000000000003",
      "-1.000000000000003",
      "1",
      "0.00000000001234567890123457",
      "10000000000000000"].join("\n"));
  });

  it("int8, p, f and decfloat34 into decfloat16", async () => {
    const code = `
    DATA lv TYPE decfloat16.
    DATA lv_i8 TYPE int8.
    DATA lv_p TYPE p LENGTH 16 DECIMALS 10.
    DATA lv_f TYPE f.
    DATA lv_34 TYPE decfloat34.
    lv_i8 = 1234567890123456789.
    lv = lv_i8.
    WRITE / |{ lv }|.
    lv_p = '123456.1234567890'.
    lv = lv_p.
    WRITE / |{ lv }|.
    lv_f = 1 / 3.
    lv = lv_f.
    WRITE / |{ lv }|.
    lv_34 = '1.234567890123456789012345678901234'.
    lv = lv_34.
    WRITE / |{ lv }|.
    lv_34 = lv.
    WRITE / |{ lv_34 }|.
    lv = CONV decfloat16( '2.718281828459045235' ).
    WRITE / |{ lv }|.`;
    expect(await runAndGet(code)).to.equal([
      "1234567890123457000",
      "123456.123456789",
      "0.3333333333333333",
      "1.234567890123457",
      "1.234567890123457",
      "2.718281828459045"].join("\n"));
  });

  it("CREATE DATA TYPE decfloat16, static and dynamic", async () => {
    const code = `
    DATA lr TYPE REF TO data.
    FIELD-SYMBOLS <fs> TYPE any.
    CREATE DATA lr TYPE decfloat16.
    ASSIGN lr->* TO <fs>.
    <fs> = '1234567890123456789'.
    WRITE / |{ <fs> }|.
    CREATE DATA lr TYPE ('DECFLOAT16').
    ASSIGN lr->* TO <fs>.
    <fs> = '1234567890123456789'.
    WRITE / |{ <fs> }|.`;
    expect(await runAndGet(code)).to.equal("1234567890123457000\n1234567890123457000");
  });

});
