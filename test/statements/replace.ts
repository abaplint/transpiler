import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - REPLACE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("REPLACE ALL", async () => {
    const code = `
  DATA str TYPE string.
  str = 'aabbccbb'.
  REPLACE ALL OCCURRENCES OF |bb| IN str WITH |dd|.
  ASSERT str = 'aaddccdd'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE, dump/throw exception", async () => {
    const code = `
  DATA lv_string TYPE string.
  lv_string = 'foobar'.
  REPLACE ALL OCCURRENCES OF '' IN lv_string WITH 'sdfs'.
  ASSERT lv_string = 'foobar'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch (e) {
      expect(e).to.contain("REPLACE, zero length input");
    }
  });

  it("REPLACE identical contents", async () => {
    const code = `
  DATA lv_string TYPE string.
  lv_string = 'ab'.
  REPLACE ALL OCCURRENCES OF 'ab' IN lv_string WITH \`ab\`.
  ASSERT lv_string = 'ab'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE FIRST", async () => {
    const code = `
  DATA lv_host TYPE string.
  lv_host = 'abc'.
  REPLACE FIRST OCCURRENCE OF '' IN lv_host WITH ''.
  ASSERT lv_host = 'abc'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("REPLACE IN SECTION LENGTH", async () => {
    const code = `
  DATA lv_value TYPE string.
  lv_value = 'abfooab'.
  REPLACE REGEX 'ab' IN SECTION LENGTH 2 OF lv_value WITH ''.
  WRITE lv_value.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("fooab");
  });

});