import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - utclong type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("just the DATA", async () => {
    const code = `DATA foo TYPE utclong.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("set utclong from string", async () => {
    const code = `
      DATA foo TYPE utclong.
      foo = '2026-04-27 12:00:00.0000000'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("set utclong from character", () => {
    const abap2 = new ABAP({console: new MemoryConsole()});
    const foo = new abap2.types.UTCLong();
    const bar = new abap2.types.Character(27);
    bar.set("2026-04-27 12:00:00.0000000");
    foo.set(bar);
    expect(foo.get()).to.equal("2026-04-27 12:00:00.0000000");
  });
});

