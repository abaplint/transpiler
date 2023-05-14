import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zdecfloat34.prog.abap", contents}]);
}

describe("Running Examples - Decfloat34 type", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("basic", async () => {
    const code = `
DATA foo TYPE decfloat34.
foo = 1.
WRITE / foo.

foo = '1.12345'.
WRITE / foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1,12345");
  });

  it("multiply", async () => {
    const code = `
    DATA dec TYPE decfloat34.
    dec = 1 / 2.
    dec = dec * 10.
    WRITE / dec.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("compare, LT", async () => {
    const code = `
    DATA val1 TYPE decfloat34.
    DATA val2 TYPE decfloat34.
    val1 = 20.
    val2 = 100.
    ASSERT val1 LT val2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("compare, LT, field symbols", async () => {
    const code = `
    DATA val1 TYPE decfloat34.
    DATA val2 TYPE decfloat34.
    FIELD-SYMBOLS <fs1> TYPE decfloat34.
    FIELD-SYMBOLS <fs2> TYPE decfloat34.
    val1 = 20.
    val2 = 100.
    ASSIGN val1 TO <fs1>.
    ASSIGN val2 TO <fs2>.
    ASSERT <fs1> LT <fs2>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("compare, EQ", async () => {
    const code = `
    DATA val1 TYPE decfloat34.
    DATA val2 TYPE decfloat34.
    val1 = 100.
    val2 = 100.
    ASSERT val1 EQ val2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("thousand", async () => {
    const code = `
DATA foo TYPE decfloat34.
DATA out TYPE string.
foo = 1000.
out = |{ foo }|.
WRITE / out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1000");
  });

  it("parsing", async () => {
    const code = `
DATA foo TYPE decfloat34.
DATA out TYPE string.
foo = '1.123'.
out = |{ foo }|.
WRITE / foo.
WRITE / out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1,123\n1.123");
  });

});
