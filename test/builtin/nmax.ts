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

});
