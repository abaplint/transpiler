import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Time type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Time initial value", async () => {
    const code = `
      DATA time TYPE t.
      WRITE time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("000000");
  });

  it("Time initial value", async () => {
    const code = `
      DATA time TYPE t.
      ASSERT time IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Time, assignment from character type", async () => {
    const code = `
      DATA time TYPE t.
      time = '123456'.
      WRITE time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123456");
  });

  it("Time, assignment from numeric type", async () => {
    const code = `
      DATA time TYPE t.
      time = 123456.
      WRITE time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("101736");
  });

  it("Time, adding 1", async () => {
    const code = `
      DATA time TYPE t.
      time = '000241'.
      time = time + 1.
      WRITE time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("000242");
  });

  it("sy uzeit is set", async () => {
    const code = `WRITE sy-uzeit.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.not.equal("000000");
  });

  it("time, offset set", async () => {
    const code = `
    DATA clock TYPE t.
    clock(2) = 8.
    WRITE / clock.
    WRITE / clock(2).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("080000\n08");
  });

  it("time, empty from string", async () => {
    const code = `
    DATA iv_value TYPE string.
    DATA rv_result TYPE t.
    rv_result = '112233'.
    rv_result = iv_value.
    WRITE / rv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("000000");
  });

  it("time, from string, A", async () => {
    const code = `
    DATA iv_value TYPE string.
    DATA rv_result TYPE t.
    rv_result = '112233'.
    iv_value = |A|.
    rv_result = iv_value.
    WRITE / rv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("A00000");
  });

  it("time, from string, ABC", async () => {
    const code = `
    DATA iv_value TYPE string.
    DATA rv_result TYPE t.
    rv_result = '112233'.
    iv_value = |ABCABCABC|.
    rv_result = iv_value.
    WRITE / rv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ABCABC");
  });

});