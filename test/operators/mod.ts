import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - MOD", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("integer MOD", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 MOD 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("integer MOD, lower case", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 mod 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("mod, negative value", async () => {
    const code = `
    DATA n TYPE i.
    n = -4 MOD 3.
    WRITE / n.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("MOD, both negative", async () => {
    const code = `
    DATA int TYPE i.
    int = -5 MOD -2.
    ASSERT int = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("MOD, negative", async () => {
    const code = `
    DATA int TYPE i.
    int = 5 MOD -2.
    ASSERT int = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});