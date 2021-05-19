import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin Numeric Extremum Functions, nmin & nmax", () => {

  beforeEach(async () => {
    abap = new ABAP();
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

});
