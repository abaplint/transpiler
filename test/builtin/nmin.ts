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

});
