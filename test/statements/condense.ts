import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CONDENSE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("CONDENSE", async () => {
    const code = `
      DATA str TYPE string.
      str = |   fo  o b   ar |.
      ASSERT str = |   fo  o b   ar |.
      CONDENSE str.
      ASSERT str = |fo o b ar|.
      CONDENSE str NO-GAPS.
      ASSERT str = |foobar|.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("CONDENSE converted type", async () => {
    const code = `
      DATA lv_len TYPE i.
      DATA lv_char10 TYPE c LENGTH 10.
      lv_len = 5.
      lv_char10 = lv_len.
      CONDENSE lv_char10.
      WRITE lv_char10.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("CONDENSE int'ed string", async () => {
    const code = `
      DATA lv_char10 TYPE c LENGTH 10.
      lv_char10 = 5.
      CONDENSE lv_char10.
      WRITE lv_char10.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("5");
  });

});