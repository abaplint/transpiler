// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - translate", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("translate() 1", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = |foo| from = |o| to = |u| ).
ASSERT foo = 'fuu'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("translate() 2", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = |foo| from = |o| to = || ).
ASSERT foo = 'f'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("translate() 3", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = |foo| from = |fo| to = |ba| ).
ASSERT foo = 'baa'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("translate(), empty", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = |foo| from = || to = || ).
ASSERT foo = 'foo'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("translate(), should single step", async () => {
    const code = `
DATA foo TYPE string.
foo = translate( val = |a| from = |ab| to = |bc| ).
ASSERT foo = 'b'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
