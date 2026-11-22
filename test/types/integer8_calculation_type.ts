import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

async function runAndOutput(code: string): Promise<string> {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  await f(abap);
  return abap.console.get();
}

// The target field is part of the calculation type: with an int8 target and
// operands of type i, the calculation type is int8, and i * i is exact.
// A double holds it exactly only up to 2^53.
// Measured on a system: "i * i above 2^53" (9007199515875289) and the divisions
// marked below. The other expected values are exact int8 arithmetic, UNMEASURED.
describe("Running Examples - int8 target, calculation type int8", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("i * i above 2^53", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_b TYPE i.
DATA lv_8 TYPE int8.
lv_a = 94906267.
lv_b = 94906267.
lv_8 = lv_a * lv_b.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("9007199515875289");
  });

  it("UNMEASURED: max * max", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_a = 2147483647.
lv_8 = lv_a * lv_a.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("4611686014132420609");
  });

  it("UNMEASURED: i * constant", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_a = 94906267.
lv_8 = lv_a * 94906267.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("9007199515875289");
  });

  it("UNMEASURED: negated product", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_a = 94906267.
lv_8 = - lv_a * lv_a.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("-9007199515875289");
  });

  it("UNMEASURED: product minus product", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_b TYPE i.
DATA lv_c TYPE i.
DATA lv_8 TYPE int8.
lv_a = 94906267.
lv_b = 3.
lv_c = 5.
lv_8 = lv_a * lv_a - lv_b * lv_c.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("9007199515875274");
  });

  it("UNMEASURED: parentheses", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_a = 94906267.
lv_8 = ( lv_a + lv_a ) * lv_a.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("18014399031750578");
  });

  it("UNMEASURED: product DIV and MOD", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_a = 94906267.
lv_8 = lv_a * lv_a DIV 3.
WRITE / lv_8.
lv_8 = lv_a * lv_a MOD 1000.
WRITE / lv_8.`;
    expect(await runAndOutput(code)).to.equal("3002399838625096\n289");
  });

  it("UNMEASURED: += with a product", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_a = 94906267.
lv_8 = 0.
lv_8 += lv_a * lv_a.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("9007199515875289");
  });

  it("UNMEASURED: field symbol operand", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
FIELD-SYMBOLS <lv_a> TYPE i.
lv_a = 94906267.
ASSIGN lv_a TO <lv_a>.
lv_8 = <lv_a> * <lv_a>.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("9007199515875289");
  });

  // measured on a system: int8 operands 7 and 2 give 4, -7 and 2 give -4
  it("int8 / int8 into int8 rounds half away from zero", async () => {
    const code = `
DATA lv_x TYPE int8.
DATA lv_y TYPE int8.
DATA lv_8 TYPE int8.
lv_x = 7.
lv_y = 2.
lv_8 = lv_x / lv_y.
WRITE / lv_8.
lv_x = -7.
lv_8 = lv_x / lv_y.
WRITE / lv_8.`;
    expect(await runAndOutput(code)).to.equal("4\n-4");
  });

  // measured on a system: 7 / 2 gives 4, -7 / 2 gives -4, 5 / 2 gives 3
  it("i / i into int8 rounds half away from zero", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_b TYPE i.
DATA lv_8 TYPE int8.
lv_b = 2.
lv_a = 7.
lv_8 = lv_a / lv_b.
WRITE / lv_8.
lv_a = -7.
lv_8 = lv_a / lv_b.
WRITE / lv_8.
lv_a = 5.
lv_8 = lv_a / lv_b.
WRITE / lv_8.`;
    expect(await runAndOutput(code)).to.equal("4\n-4\n3");
  });

  // UNMEASURED: an int8 operand makes the calculation type int8 also with an i target,
  // so "/" rounds there too
  it("UNMEASURED: int8 / int8 into i rounds half away from zero", async () => {
    const code = `
DATA lv_x TYPE int8.
DATA lv_y TYPE int8.
DATA lv_i TYPE i.
lv_x = 7.
lv_y = 2.
lv_i = lv_x / lv_y.
WRITE lv_i.`;
    expect(await runAndOutput(code)).to.equal("4");
  });

  // The target is part of the calculation type: with an f target it is f, the fraction is kept
  it("int8 / int8 into f", async () => {
    const code = `
DATA lv_x TYPE int8.
DATA lv_y TYPE int8.
DATA lv_f TYPE f.
lv_x = 7.
lv_y = 2.
lv_f = lv_x / lv_y.
ASSERT lv_f = '3.5'.`;
    await runAndOutput(code);
  });

  // with a p target the calculation type is p, the fraction is kept up to the decimals of the target
  it("int8 / int8 into p", async () => {
    const code = `
DATA lv_x TYPE int8.
DATA lv_y TYPE int8.
DATA lv_p TYPE p LENGTH 8 DECIMALS 1.
lv_x = 7.
lv_y = 2.
lv_p = lv_x / lv_y.
ASSERT lv_p = '3.5'.`;
    await runAndOutput(code);
  });

  it("int8 / constant into p DECIMALS 2", async () => {
    const code = `
DATA lv_p TYPE p LENGTH 8 DECIMALS 2.
DATA lv_8 TYPE int8 VALUE 7.
lv_p = lv_8 / 2.
ASSERT lv_p = '3.50'.`;
    await runAndOutput(code);
  });

  it("int8 MOD then / into p", async () => {
    const code = `
DATA lv_p TYPE p LENGTH 8 DECIMALS 2.
DATA lv_8 TYPE int8 VALUE 7.
lv_p = lv_8 MOD 100 / 2.
ASSERT lv_p = '3.50'.`;
    await runAndOutput(code);
  });

  it("int8 / into p, negative", async () => {
    const code = `
DATA lv_p TYPE p LENGTH 8 DECIMALS 2.
DATA lv_8 TYPE int8 VALUE -7.
lv_p = lv_8 / 4.
ASSERT lv_p = '-1.75'.`;
    await runAndOutput(code);
  });

  it("int8 method result MOD then / into p", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS next RETURNING VALUE(rv) TYPE int8.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD next.
    rv = 1234567890123.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lv_p TYPE p LENGTH 8 DECIMALS 2.
  lv_p = lcl=>next( ) MOD 100000000 / 100.
  ASSERT lv_p = '678901.23'.`;
    await runAndOutput(code);
  });

  // in a comparison there is no target, the calculation type is int8 and "/" rounds
  it("int8 / in a comparison rounds", async () => {
    const code = `
DATA lv_8 TYPE int8 VALUE 7.
IF lv_8 / 2 = 4.
  WRITE 'yes'.
ELSE.
  WRITE 'no'.
ENDIF.`;
    expect(await runAndOutput(code)).to.equal("yes");
  });

  it("int8 / in a comparison rounds, negative", async () => {
    const code = `
DATA lv_8 TYPE int8 VALUE -7.
IF lv_8 / 2 = -4.
  WRITE 'yes'.
ELSE.
  WRITE 'no'.
ENDIF.`;
    expect(await runAndOutput(code)).to.equal("yes");
  });

  // int8 calculation type rounds every intermediate quotient: 7 / 2 is 4, times 2 is 8
  it("UNMEASURED: intermediate rounding, a / b * b into int8", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_b TYPE i.
DATA lv_8 TYPE int8.
lv_a = 7.
lv_b = 2.
lv_8 = lv_a / lv_b * lv_b.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("8");
  });

  it("UNMEASURED: an f operand keeps calculation type f", async () => {
    const code = `
DATA lv_f TYPE f.
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_f = '2.5'.
lv_a = 3.
lv_8 = lv_f * lv_a.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("8");
  });

  it("UNMEASURED: an i target is unchanged", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_i TYPE i.
lv_a = 46340.
lv_i = lv_a * lv_a.
WRITE lv_i.`;
    expect(await runAndOutput(code)).to.equal("2147395600");
  });

});
