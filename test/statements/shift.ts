import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - SHIFT", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("shift 1 places left", async () => {
    const code = `
  DATA lv_temp TYPE string.
  lv_temp = 'abc'.
  SHIFT lv_temp BY 1 PLACES LEFT.
  WRITE lv_temp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bc");
  });

  it("shift up to left, found", async () => {
    const code = `
  DATA lv_temp TYPE string.
  lv_temp = 'abc/bar'.
  SHIFT lv_temp UP TO '/' LEFT.
  WRITE lv_temp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("/bar");
  });

  it("shift up to left, not found", async () => {
    const code = `
  DATA lv_temp TYPE string.
  lv_temp = 'abcbar'.
  SHIFT lv_temp UP TO '/' LEFT.
  WRITE lv_temp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("abcbar");
  });

  it("SHIFT LEFT", async () => {
    const code = `
  DATA foo TYPE c LENGTH 10.
  foo = '11223355'.
  SHIFT foo LEFT DELETING LEADING '12'.
  WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3355");
  });

});