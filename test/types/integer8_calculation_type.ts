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

  it("max * max", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_a = 2147483647.
lv_8 = lv_a * lv_a.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("4611686014132420609");
  });

  it("i * constant", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_a = 94906267.
lv_8 = lv_a * 94906267.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("9007199515875289");
  });

  it("negated product", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_a = 94906267.
lv_8 = - lv_a * lv_a.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("-9007199515875289");
  });

  it("product minus product", async () => {
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

  it("parentheses", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_a = 94906267.
lv_8 = ( lv_a + lv_a ) * lv_a.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("18014399031750578");
  });

  it("product DIV and MOD", async () => {
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

  it("+= with a product", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_8 TYPE int8.
lv_a = 94906267.
lv_8 = 0.
lv_8 += lv_a * lv_a.
WRITE lv_8.`;
    expect(await runAndOutput(code)).to.equal("9007199515875289");
  });

  it("field symbol operand", async () => {
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
  it("int8 / int8 into i rounds half away from zero", async () => {
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

  // UNMEASURED: an f target makes the calculation type f, the int8 rounding does not apply
  it("int8 / int8 into f does not round to a whole number", async () => {
    const code = `
DATA lv_x TYPE int8.
DATA lv_y TYPE int8.
DATA lv_f TYPE f.
lv_x = 7.
lv_y = 2.
lv_f = lv_x / lv_y.
ASSERT lv_f < 4.`;
    await runAndOutput(code);
  });

  it("an f operand keeps calculation type f", async () => {
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

  it("an i target is unchanged", async () => {
    const code = `
DATA lv_a TYPE i.
DATA lv_i TYPE i.
lv_a = 46340.
lv_i = lv_a * lv_a.
WRITE lv_i.`;
    expect(await runAndOutput(code)).to.equal("2147395600");
  });

});
