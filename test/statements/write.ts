import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - WRITE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("WRITE simple", async () => {
    const code = `
    WRITE: 'aaa'.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("aaa");
  });


  it("WRITE with new line", async () => {
    const code = `
    WRITE: / 'aaa'.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("aaa");
  });

  it("WRITE with new line - two lines", async () => {
    const code = `
    WRITE: / 'aaa'.
    WRITE: / 'bbb'.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("aaa\nbbb");
  });


  it("WRITE with empty line - two lines", async () => {
    const code = `
    WRITE: / ''.
    WRITE: / 'bbb'.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bbb");
  });

  it("WRITE with no line - one lines", async () => {
    const code = `
    WRITE: / .
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("\n");
  });

  it("WRITE with no line - two lines", async () => {
    const code = `
    WRITE: / .
    WRITE: / 'bbb'.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("\n\nbbb");
  });

});