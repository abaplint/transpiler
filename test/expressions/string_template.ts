import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running expressions - String templates", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Output TIMESTAMP = ISO", async () => {
    const code = `
    CONSTANTS lc_epoch TYPE p VALUE '19700101000000'.
    WRITE |{ lc_epoch TIMESTAMP = ISO }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1970-01-01T00:00:00");
  });

  it("Output DATE = ISO", async () => {
    const code = `
    CONSTANTS date TYPE d VALUE '20210321'.
    WRITE |{ date DATE = ISO }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2021-03-21");
  });

  it("Output TIME = ISO", async () => {
    const code = `
    CONSTANTS time TYPE t VALUE '112233'.
    WRITE |{ time TIME = ISO }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("11:22:33");
  });

  it("Output WIDTH, add spaces", async () => {
    const code = `
    DATA row TYPE c LENGTH 1.
    DATA result TYPE string.
    result = |{ row WIDTH = 10 }|.
    WRITE strlen( result ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("Output WIDTH, nothing to add", async () => {
    const code = `
    DATA row TYPE string.
    DATA result TYPE string.
    row = 'abcde'.
    result = |{ row WIDTH = 2 }|.
    WRITE result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("abcde");
  });

  it("More Output WIDTH", async () => {
    const code = `
    DATA row TYPE c LENGTH 255.
    DATA width TYPE i.
    width = 2.
    DATA result TYPE string.
    result = |{ row WIDTH = width }|.
    ASSERT result = \`  \`.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});