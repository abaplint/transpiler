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

  it("Builtin numerical: nmin 1", async () => {
    const code = `
      DATA int1 TYPE i VALUE 1.
      DATA int2 TYPE i VALUE 2.
      DATA min TYPE i.
      min = nmin( val1 = int1
                  val2 = int2 ).
      WRITE / min.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("Builtin numerical: nmin 2", async () => {
    const code = `
      DATA int1 TYPE i VALUE 42.
      DATA int2 TYPE i VALUE 37.
      DATA min TYPE i.
      min = nmin( val1 = int1
                  val2 = 99
                  val3 = 156
                  val4 = 234
                  val5 = 777
                  val6 = int2
                  val7 = 200000 ).
      WRITE / min.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("37");
  });

  it("packed", async () => {
    const code = `
    DATA total1 TYPE p LENGTH 3 DECIMALS 2.
    DATA total2 TYPE p LENGTH 3 DECIMALS 2.
    total1 = 999.
    total2 = '15.2'.
    total1 = nmin( val1 = total1 val2 = total2 ).
    WRITE total1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("15,20");
  });

  it("integer arguments give an integer, DO counts with it", async () => {
    const code = `
      DATA a TYPE i VALUE 3.
      DATA b TYPE i VALUE 5.
      DATA count TYPE i.
      DO nmin( val1 = a val2 = b ) TIMES.
        count = count + 1.
      ENDDO.
      WRITE / count.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("packed arguments give a packed result with their decimals", async () => {
    const code = `
      DATA p1 TYPE p LENGTH 8 DECIMALS 1 VALUE '2.5'.
      DATA p2 TYPE p LENGTH 8 DECIMALS 3 VALUE '7.125'.
      WRITE / |{ nmin( val1 = p1 val2 = p2 ) }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2.500");
  });

});
