import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - nmin", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Builtin numerical: nmax 1", async () => {
    const code = `
      DATA int1 TYPE i VALUE 1.
      DATA int2 TYPE i VALUE 2.
      DATA max TYPE i.
      max = nmax( val1 = int1
                  val2 = int2 ).
      WRITE / max.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("Builtin numerical: nmax 2", async () => {
    const code = `
      DATA int1 TYPE i VALUE 42.
      DATA int2 TYPE i VALUE 37.
      DATA max TYPE i.
      max = nmax( val1 = int1
                  val2 = 234
                  val3 = 156
                  val4 = 200000
                  val5 = 777
                  val6 = int2
                  val7 = 3233 ).
      WRITE / max.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("200000");
  });

  it("packed", async () => {
    const code = `
    DATA total1 TYPE p LENGTH 3 DECIMALS 2.
    DATA total2 TYPE p LENGTH 3 DECIMALS 2.
    total1 = '15.3'.
    total2 = '15.2'.
    total1 = nmax( val1 = total1 val2 = total2 ).
    WRITE total1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("15,30");
  });

  it("packed arguments give a packed result", async () => {
    const code = `
      DATA total1 TYPE p LENGTH 3 DECIMALS 2 VALUE '15.3'.
      DATA total2 TYPE p LENGTH 3 DECIMALS 2 VALUE '15.2'.
      WRITE / |{ nmax( val1 = total1 val2 = total2 ) }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("15.30");
  });

  it("a decfloat34 argument gives a decfloat34", async () => {
    const code = `
      DATA d TYPE decfloat34 VALUE '1.25'.
      DATA i TYPE i VALUE 1.
      WRITE / |{ nmax( val1 = i val2 = d ) }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1.25");
  });

  it("int8 arguments above 2^53, exact", async () => {
    const code = `
  DATA a TYPE int8.
  DATA b TYPE int8.
  DATA r TYPE int8.
  DATA p TYPE p LENGTH 16 DECIMALS 1 VALUE '1.5'.
  DATA rp TYPE p LENGTH 16 DECIMALS 1.
  a = 9223372036854775806.
  b = 9223372036854775807.
  r = nmax( val1 = a val2 = b ).
  WRITE / |{ r }|.
  r = nmin( val1 = a val2 = b ).
  WRITE / |{ r }|.
  r = nmax( val1 = b val2 = 1 ).
  WRITE / |{ r }|.
  a = -9223372036854775807.
  a = a - 1.
  r = nmin( val1 = a val2 = 1 ).
  WRITE / |{ r }|.
  rp = nmax( val1 = b val2 = p ).
  WRITE / |{ rp }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(
      "9223372036854775807\n9223372036854775806\n9223372036854775807\n-9223372036854775808\n9223372036854775807.0");
  });

});
