import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - GET RUN TIME", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("simple", async () => {
    const code = `
    DATA lv_start TYPE i.
    DATA lv_end TYPE i.
    DATA calc TYPE i.
    GET RUN TIME FIELD lv_start.
    ASSERT lv_start = 0.
    DO 1000 TIMES.
      calc = 2 * 2 * 2.
      WRITE calc.
    ENDDO.
    GET RUN TIME FIELD lv_end.
    ASSERT lv_end <> 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });


  it("microseconds since the first call", async () => {
    const code = `
    DATA lv_t0 TYPE i.
    DATA lv_t1 TYPE i.
    DATA lv_t2 TYPE i.
    DATA lv_sec TYPE p LENGTH 8 DECIMALS 1 VALUE '0.1'.
    GET RUN TIME FIELD lv_t0.
    WAIT UP TO lv_sec SECONDS.
    GET RUN TIME FIELD lv_t1.
    WAIT UP TO lv_sec SECONDS.
    GET RUN TIME FIELD lv_t2.
    WRITE / lv_t0.
    WRITE / lv_t1.
    WRITE / lv_t2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    const [t0, t1, t2] = abap.console.get().split("\n").map(s => parseInt(s, 10));
    expect(t0).to.equal(0);
    // 0.1 seconds are 100000 microseconds, a timer may fire a little early
    expect(t1).to.be.greaterThan(90000);
    expect(t1).to.be.lessThan(10000000);
    expect(t2 - t1).to.be.greaterThan(90000);
  });

  it("the first call in each internal session gives 0", async () => {
    const code = `
    DATA lv_t TYPE i.
    DATA lv_sec TYPE p LENGTH 8 DECIMALS 2 VALUE '0.05'.
    GET RUN TIME FIELD lv_t.
    WRITE lv_t.
    WAIT UP TO lv_sec SECONDS.`;
    const js = await run(code);
    for (let i = 0; i < 2; i++) {
      abap = new ABAP({console: new MemoryConsole()});
      const f = new AsyncFunction("abap", js);
      await f(abap);
      expect(abap.console.get()).to.equal("0");
    }
  });

  it("never goes backwards", async () => {
    const code = `
    DATA lv_prev TYPE i.
    DATA lv_t TYPE i.
    DO 20000 TIMES.
      GET RUN TIME FIELD lv_t.
      ASSERT lv_t >= lv_prev.
      lv_prev = lv_t.
    ENDDO.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});